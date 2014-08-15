/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.refactoring.rules;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Iterator;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.IRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeMemberDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.ConstructorInvocation;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.EnumConstantDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.MarkerAnnotation;
import org.eclipse.jdt.core.dom.MemberRef;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.MethodRef;
import org.eclipse.jdt.core.dom.MethodRefParameter;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.QualifiedType;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleMemberAnnotation;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.SynchronizedStatement;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TextElement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclarationStatement;
import org.eclipse.jdt.core.dom.TypeLiteral;
import org.eclipse.jdt.core.dom.TypeParameter;
import org.eclipse.jdt.core.dom.UnionType;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jdt.core.dom.WildcardType;

import static org.autorefactor.AutoRefactorPlugin.*;
import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Aggregates running several visitors into only one visitor to increase performances.
 * When one visitor refactors a subtree of the AST, visitors coming after will not be able to visit it.
 * Visitors throwing exceptions are isolated and ignored for the rest of a run for stability.
 */
public class AggregateASTVisitor extends ASTVisitor implements IJavaRefactoring {

    private Map<Class<?>, List<ASTVisitor>> visitorsMap = new HashMap<Class<?>, List<ASTVisitor>>();
    private Map<Class<?>, List<ASTVisitor>> endVisitorsMap = new HashMap<Class<?>, List<ASTVisitor>>();
    private List<ASTVisitor> preVisitors = new ArrayList<ASTVisitor>();
    private List<ASTVisitor> preVisitors2 = new ArrayList<ASTVisitor>();
    private List<ASTVisitor> postVisitors = new ArrayList<ASTVisitor>();
    private final List<ASTVisitor> visitors;
    private boolean debugModeOn;
    private RefactoringContext ctx;
    private List<ASTVisitor> visitorsContributingRefactoring = new ArrayList<ASTVisitor>();

    @SuppressWarnings("rawtypes")
    public AggregateASTVisitor(List<IRefactoring> visitors, boolean debugModeOn) {
        this.visitors = (List) visitors;
        this.debugModeOn = debugModeOn;
        analyzeVisitors();
    }

    public AggregateASTVisitor(IRefactoring visitor) {
        this.visitors = Arrays.asList((ASTVisitor) visitor);
        this.debugModeOn = true;
        analyzeVisitors();
    }

    private void analyzeVisitors() {
        for (ASTVisitor v : this.visitors) {
            for (Method m : v.getClass().getDeclaredMethods()) {
                if (is("preVisit", m)) {
                    preVisitors.add(v);
                } else if (is("preVisit2", m)) {
                    preVisitors2.add(v);
                } else if (is("postVisit", m)) {
                    postVisitors.add(v);
                } else if (isVisit(m)) {
                    put(visitorsMap, m.getParameterTypes()[0], v);
                } else if (isEndVisit(m)) {
                    put(endVisitorsMap, m.getParameterTypes()[0], v);
                }
            }
        }
    }

    private static boolean is(String methodName, Method m) {
        return methodName.equals(m.getName())
            && m.getParameterTypes().length == 1
            && ASTNode.class.equals(m.getParameterTypes()[0]);
    }

    private static boolean isVisit(Method m) {
        return "visit".equals(m.getName())
            && m.getParameterTypes().length == 1
            && ASTNode.class.isAssignableFrom(m.getParameterTypes()[0])
            && !Modifier.isAbstract(m.getParameterTypes()[0].getModifiers());
    }

    private static boolean isEndVisit(Method m) {
        return "endVisit".equals(m.getName())
            && m.getParameterTypes().length == 1
            && ASTNode.class.isAssignableFrom(m.getParameterTypes()[0])
            && !Modifier.isAbstract(m.getParameterTypes()[0].getModifiers());
    }

    private void put(Map<Class<?>, List<ASTVisitor>> map, Class<?> key, ASTVisitor value) {
        List<ASTVisitor> visitors = map.get(key);
        if (visitors == null) {
            visitors = new ArrayList<ASTVisitor>(1);
            map.put(key, visitors);
        }
        visitors.add(value);
    }

    private List<ASTVisitor> getVisitors(Map<Class<?>, List<ASTVisitor>> map,
            Class<? extends ASTNode> clazzKey) {
        final List<ASTVisitor> result = map.get(clazzKey);
        if (result != null) {
            return result;
        }
        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
        for (IRefactoring v : (List<IRefactoring>) (List) visitors) {
            v.setRefactoringContext(ctx);
        }
        this.visitorsContributingRefactoring.clear();
    }

    /** {@inheritDoc} */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }

    public List<ASTVisitor> getVisitorsContributingRefactoring() {
        return visitorsContributingRefactoring;
    }

    /**
     * Verify whether the following visitors can visit the current node.
     *
     * @param continueVisiting whether the current visitor reported it wants
     *        to visit the subtree of the current node
     * @param v the current visitor
     * @return true if the following visitors can visit the current node,
     *         false otherwise
     */
    private boolean continueVisiting(boolean continueVisiting, ASTVisitor v) {
        if (!continueVisiting) {
            if (!this.ctx.getRefactorings().hasRefactorings()) {
                logBadlyBehavedVisitor(v);
            } else {
                visitorsContributingRefactoring.add(v);
            }
            // changes will be made to this node.
            // no other visitors can make any more changes to it
            // => do not let other visitors visit this node
            return false;
        }
        return true;
    }

    private void logBadlyBehavedVisitor(ASTVisitor v) {
        String message = "Visitor " + v.getClass().getName() + " is badly behaved:"
            + " it reported doing a refactoring, but it did not actually contribute any refactoring.";
        if (debugModeOn) {
            throw new RuntimeException(message);
        }
        final ILog log = AutoRefactorPlugin.getDefault().getLog();
        log.log(new Status(IStatus.ERROR, PLUGIN_ID, message));
    }

    private void logFaultyVisitor(final ASTVisitor v, Exception e) {
        String message = "Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run.";
        if (debugModeOn) {
            if (e instanceof RuntimeException) {
                throw (RuntimeException) e;
            }
            throw new RuntimeException(message, e);
        }
        final ILog log = AutoRefactorPlugin.getDefault().getLog();
        log.log(new Status(IStatus.ERROR, PLUGIN_ID, message, e));
    }

    /**
     * Generates the code for all the ASTVisitor methods that delegate to the underlying visitors.
     */
    public static void main(String[] args) {
        final Method[] mm = ASTVisitor.class.getDeclaredMethods();
        Arrays.sort(mm, new Comparator<Method>() {

            public int compare(Method o1, Method o2) {
                return o1.getName().compareTo(o2.getName());
            }
        });
        for (Method m : mm) {
            System.out.println("@Override");
            System.out.print("public " + m.getReturnType() + " ");
            System.out.print(m.getName() + "(");
            Class<?>[] paramTypes = m.getParameterTypes();
            for (int i = 0; i < paramTypes.length; i++) {
                Class<?> paramType = paramTypes[i];
                if (i > 0) {
                    System.out.print(", ");
                }
                System.out.print(paramType.getSimpleName() + " node");
            }
            System.out.println(") {");
            System.out.print("\tfor (Iterator<ASTVisitor> iter = " );
            if (is("preVisit", m)) {
                System.out.print("preVisitors");
            } else if (is("preVisit2", m)) {
                System.out.print("preVisitors2");
            } else if (is("postVisit", m)) {
                System.out.print("postVisitors");
            } else if (isVisit(m)) {
                System.out.print("getVisitors(visitorsMap, " + m.getParameterTypes()[0].getSimpleName() + ".class)");
            } else if (isEndVisit(m)) {
                System.out.print("getVisitors(endVisitorsMap, " + m.getParameterTypes()[0].getSimpleName() + ".class)");
            } else {
                throw new NotImplementedException("for method " + m);
            }
            System.out.println(".iterator(); iter.hasNext();) {");
            System.out.println("\t\tfinal ASTVisitor v = iter.next();");
            System.out.println("\t\ttry {");
            if (Boolean.TYPE.equals(m.getReturnType())) {
                System.out.println("\t\t\tif (!continueVisiting(v." + m.getName() + "(node), v)) {");
                System.out.println("\t\t\t\treturn DO_NOT_VISIT_SUBTREE;");
                System.out.println("\t\t\t}");
            } else {
                System.out.println("\t\t\tv." + m.getName() + "(node);");
            }
            System.out.println("\t\t} catch (Exception e) {");
            System.out.println("\t\t\tlogFaultyVisitor(v, e);");
            System.out.println("\t\t\titer.remove();");
            System.out.println("\t\t}");
            System.out.println("\t}");
            if (Boolean.TYPE.equals(m.getReturnType())) {
                System.out.println("\treturn VISIT_SUBTREE;");
            }
            System.out.println("}");
            System.out.println();
        }
    }

    @Override
    public void endVisit(ExpressionStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ExpressionStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(FieldAccess node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, FieldAccess.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(EnumDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, EnumDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(FieldDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, FieldDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ForStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ForStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(IfStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, IfStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ContinueStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ContinueStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(DoStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, DoStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(EmptyStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, EmptyStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(EnhancedForStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, EnhancedForStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(EnumConstantDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, EnumConstantDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(LabeledStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, LabeledStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(LineComment node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, LineComment.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(MarkerAnnotation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, MarkerAnnotation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(MemberRef node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, MemberRef.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(MemberValuePair node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, MemberValuePair.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ImportDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ImportDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(InfixExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, InfixExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(InstanceofExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, InstanceofExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(Initializer node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, Initializer.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(Javadoc node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, Javadoc.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ArrayCreation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ArrayCreation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ArrayInitializer node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ArrayInitializer.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ArrayType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ArrayType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(AssertStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, AssertStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(Assignment node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, Assignment.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(Block node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, Block.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(WildcardType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, WildcardType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(AnnotationTypeDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, AnnotationTypeDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(AnnotationTypeMemberDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, AnnotationTypeMemberDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(AnonymousClassDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, AnonymousClassDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ArrayAccess node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ArrayAccess.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(CharacterLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, CharacterLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ClassInstanceCreation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ClassInstanceCreation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(CompilationUnit node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, CompilationUnit.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ConditionalExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ConditionalExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ConstructorInvocation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ConstructorInvocation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(BlockComment node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, BlockComment.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(BooleanLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, BooleanLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(BreakStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, BreakStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(CastExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, CastExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(CatchClause node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, CatchClause.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(SwitchStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, SwitchStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(SynchronizedStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, SynchronizedStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(TagElement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, TagElement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(TextElement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, TextElement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ThisExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ThisExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ThrowStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ThrowStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(StringLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, StringLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(SuperConstructorInvocation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, SuperConstructorInvocation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(SuperFieldAccess node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, SuperFieldAccess.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(SuperMethodInvocation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, SuperMethodInvocation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(SwitchCase node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, SwitchCase.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(UnionType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, UnionType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(VariableDeclarationExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, VariableDeclarationExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(VariableDeclarationStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, VariableDeclarationStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(VariableDeclarationFragment node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, VariableDeclarationFragment.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(WhileStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, WhileStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(TryStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, TryStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(TypeDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, TypeDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(TypeDeclarationStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, TypeDeclarationStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(TypeLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, TypeLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(TypeParameter node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, TypeParameter.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(NormalAnnotation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, NormalAnnotation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(NullLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, NullLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(NumberLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, NumberLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(PackageDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, PackageDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ParameterizedType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ParameterizedType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ParenthesizedExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ParenthesizedExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(MethodRef node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, MethodRef.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(MethodRefParameter node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, MethodRefParameter.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(MethodDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, MethodDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(MethodInvocation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, MethodInvocation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(Modifier node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, Modifier.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(ReturnStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, ReturnStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(SimpleName node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, SimpleName.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(SimpleType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, SimpleType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(SingleMemberAnnotation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, SingleMemberAnnotation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(SingleVariableDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, SingleVariableDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(PostfixExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, PostfixExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(PrefixExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, PrefixExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(PrimitiveType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, PrimitiveType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(QualifiedName node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, QualifiedName.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void endVisit(QualifiedType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(endVisitorsMap, QualifiedType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void postVisit(ASTNode node) {
        for (Iterator<ASTVisitor> iter = postVisitors.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.postVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public void preVisit(ASTNode node) {
        for (Iterator<ASTVisitor> iter = preVisitors.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.preVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
    }

    @Override
    public boolean preVisit2(ASTNode node) {
        for (Iterator<ASTVisitor> iter = preVisitors2.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.preVisit2(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(EnumDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, EnumDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ExpressionStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ExpressionStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(FieldAccess node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, FieldAccess.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(FieldDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, FieldDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ForStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ForStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(IfStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, IfStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ContinueStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ContinueStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(DoStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, DoStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(EmptyStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, EmptyStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(EnhancedForStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, EnhancedForStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(EnumConstantDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, EnumConstantDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(LabeledStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, LabeledStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(LineComment node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, LineComment.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MarkerAnnotation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, MarkerAnnotation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MemberRef node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, MemberRef.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MemberValuePair node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, MemberValuePair.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ImportDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ImportDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(InfixExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, InfixExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(InstanceofExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, InstanceofExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Initializer node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, Initializer.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Javadoc node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, Javadoc.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ArrayCreation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ArrayCreation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ArrayInitializer node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ArrayInitializer.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ArrayType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ArrayType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(AssertStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, AssertStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Assignment node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, Assignment.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Block node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, Block.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(VariableDeclarationFragment node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, VariableDeclarationFragment.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(AnnotationTypeDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, AnnotationTypeDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(AnnotationTypeMemberDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, AnnotationTypeMemberDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(AnonymousClassDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, AnonymousClassDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ArrayAccess node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ArrayAccess.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(CharacterLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, CharacterLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ClassInstanceCreation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ClassInstanceCreation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(CompilationUnit node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, CompilationUnit.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ConditionalExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ConditionalExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ConstructorInvocation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ConstructorInvocation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(BlockComment node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, BlockComment.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(BooleanLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, BooleanLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(BreakStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, BreakStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(CastExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, CastExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(CatchClause node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, CatchClause.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SwitchStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, SwitchStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SynchronizedStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, SynchronizedStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TagElement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, TagElement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TextElement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, TextElement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ThisExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ThisExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ThrowStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ThrowStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(StringLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, StringLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SuperConstructorInvocation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, SuperConstructorInvocation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SuperFieldAccess node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, SuperFieldAccess.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SuperMethodInvocation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, SuperMethodInvocation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SwitchCase node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, SwitchCase.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(UnionType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, UnionType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(VariableDeclarationExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, VariableDeclarationExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(WildcardType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, WildcardType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(WhileStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, WhileStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(VariableDeclarationStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, VariableDeclarationStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TryStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, TryStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TypeDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, TypeDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TypeDeclarationStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, TypeDeclarationStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TypeLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, TypeLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TypeParameter node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, TypeParameter.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(NormalAnnotation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, NormalAnnotation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(NullLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, NullLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(NumberLiteral node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, NumberLiteral.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(PackageDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, PackageDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ParameterizedType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ParameterizedType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ParenthesizedExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ParenthesizedExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodRef node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, MethodRef.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodRefParameter node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, MethodRefParameter.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, MethodDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodInvocation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, MethodInvocation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Modifier node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, Modifier.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ReturnStatement node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, ReturnStatement.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SimpleName node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, SimpleName.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SimpleType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, SimpleType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SingleMemberAnnotation node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, SingleMemberAnnotation.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SingleVariableDeclaration node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, SingleVariableDeclaration.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(PostfixExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, PostfixExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(PrefixExpression node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, PrefixExpression.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(PrimitiveType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, PrimitiveType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(QualifiedName node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, QualifiedName.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(QualifiedType node) {
        for (Iterator<ASTVisitor> iter = getVisitors(visitorsMap, QualifiedType.class).iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

}
