/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.autorefactor.preferences.Preferences;
import org.autorefactor.refactoring.JavaRefactoringRule;
import org.autorefactor.refactoring.RefactoringRule;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.AutoRefactorException;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.UnhandledException;
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
public class AggregateASTVisitor extends ASTVisitor implements JavaRefactoringRule {

    private final Map<Class<?>, List<ASTVisitor>> visitorsMap = new HashMap<Class<?>, List<ASTVisitor>>();
    private final Map<Class<?>, List<ASTVisitor>> endVisitorsMap = new HashMap<Class<?>, List<ASTVisitor>>();
    private final Set<ASTVisitor> preVisitors = new LinkedHashSet<ASTVisitor>();
    private final Set<ASTVisitor> preVisitors2 = new LinkedHashSet<ASTVisitor>();
    private final Set<ASTVisitor> postVisitors = new LinkedHashSet<ASTVisitor>();

    private final List<ASTVisitor> visitors;

    private RefactoringContext ctx;
    private final List<ASTVisitor> visitorsContributingRefactoring = new ArrayList<ASTVisitor>();

    /**
     * Builds an instance of this class.
     *
     * @param visitors the visitors that will be executed by this {@link AggregateASTVisitor}
     */
    @SuppressWarnings("rawtypes")
    public AggregateASTVisitor(List<RefactoringRule> visitors) {
        this.visitors = (List) visitors;
        analyzeVisitors();
    }

    /** {@inheritDoc} */
    @Override
    public boolean isEnabled(Preferences preferences) {
        return true;
    }

    private void analyzeVisitors() {
        for (ASTVisitor v : this.visitors) {
            analyzeVisitor(v, v.getClass());
        }
    }

    private void analyzeVisitor(ASTVisitor v, Class<?> clazz) {
        if (ASTVisitor.class.equals(clazz)) {
            return;
        }
        for (Method m : clazz.getDeclaredMethods()) {
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
        analyzeVisitor(v, clazz.getSuperclass());
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
    @Override
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
        for (RefactoringRule v : (List<RefactoringRule>) (List) visitors) {
            v.setRefactoringContext(ctx);
        }
        this.visitorsContributingRefactoring.clear();
    }

    /** {@inheritDoc} */
    @Override
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }

    /**
     * Returns the visitors that contributed refactorings in the last run.
     *
     * @return the visitors that contributed refactorings in the last run
     */
    public List<ASTVisitor> getVisitorsContributingRefactoring() {
        return visitorsContributingRefactoring;
    }

    /**
     * Verify whether the following visitors can visit the current node.
     *
     * @param continueVisiting whether the current visitor reported it wants
     *        to visit the subtree of the current node
     * @param v the current visitor
     * @param node the node being currently visited
     * @return true if the following visitors can visit the current node,
     *         false otherwise
     */
    private boolean continueVisiting(boolean continueVisiting, ASTVisitor v, ASTNode node) {
        if (!continueVisiting) {
            if (!this.ctx.getRefactorings().hasRefactorings()) {
                logBadlyBehavedVisitor(v, node);
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

    private void logBadlyBehavedVisitor(ASTVisitor v, ASTNode node) {
        String message = "Visitor " + v.getClass().getName() + " is badly behaved:"
                + " it reported doing a refactoring, but it did not actually contribute any refactoring.";
        logError(message, new AutoRefactorException(node, message));
    }

    private void logFaultyVisitor(ASTVisitor v, ASTNode node, Exception e) {
        String message = "Visitor " + v.getClass().getName() + " is faulty,"
                + " it will be disabled for the rest of this run.";
        logError(message, new UnhandledException(node, message, e));
    }

    /**
     * Generates the code for all the ASTVisitor methods that delegate to the underlying visitors.
     *
     * @param args the arguments of the Java program
     */
    public static void main(String[] args) {
        final Method[] mm = ASTVisitor.class.getDeclaredMethods();
        Arrays.sort(mm, new Comparator<Method>() {
            /** {@inheritDoc} */
            @Override
            public int compare(Method o1, Method o2) {
                return o1.getName().compareTo(o2.getName());
            }
        });
        for (Method m : mm) {
            System.out.println("/** {@inheritDoc} */");
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
            final boolean isVisit = isVisit(m);
            final boolean isEndVisit = isEndVisit(m);
            final boolean isPrevisit2 = is("preVisit2", m);
            if (isVisit || isEndVisit) {
                System.out.print("\tfinal List<ASTVisitor> visitorList = getVisitors(");
                System.out.print((isVisit ? "visitorsMap" : "endVisitorsMap") + ", ");
                System.out.println(m.getParameterTypes()[0].getSimpleName() + ".class);");
            }
            System.out.print("\tfor (Iterator<ASTVisitor> iter = ");
            if (is("preVisit", m)) {
                System.out.print("preVisitors");
            } else if (isPrevisit2) {
                System.out.print("preVisitors2");
            } else if (is("postVisit", m)) {
                System.out.print("postVisitors");
            } else if (isVisit || isEndVisit) {
                System.out.print("visitorList");
            } else {
                throw new NotImplementedException(null, "for method " + m);
            }
            System.out.println(".iterator(); iter.hasNext();) {");
            System.out.println("\t\tfinal ASTVisitor v = iter.next();");
            System.out.println("\t\ttry {");
            if (isPrevisit2) {
                System.out.println("\t\t\tif (!v." + m.getName() + "(node)) {");
                System.out.println("\t\t\t\treturn DO_NOT_VISIT_SUBTREE;");
                System.out.println("\t\t\t}");
            } else if (Boolean.TYPE.equals(m.getReturnType())) {
                System.out.println("\t\t\tif (!continueVisiting(v." + m.getName() + "(node), v, node)) {");
                System.out.println("\t\t\t\treturn DO_NOT_VISIT_SUBTREE;");
                System.out.println("\t\t\t}");
            } else {
                System.out.println("\t\t\tv." + m.getName() + "(node);");
            }
            System.out.println("\t\t} catch (Exception e) {");
            System.out.println("\t\t\tlogFaultyVisitor(v, node, e);");
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

    /** {@inheritDoc} */
    @Override
    public void endVisit(AnnotationTypeDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, AnnotationTypeDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(AnnotationTypeMemberDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, AnnotationTypeMemberDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(AnonymousClassDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, AnonymousClassDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ArrayAccess node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ArrayAccess.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ArrayCreation node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ArrayCreation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ArrayInitializer node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ArrayInitializer.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ArrayType node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ArrayType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(AssertStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, AssertStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(Assignment node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, Assignment.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(Block node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, Block.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(BlockComment node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, BlockComment.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(BooleanLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, BooleanLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(BreakStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, BreakStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(CastExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, CastExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(CatchClause node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, CatchClause.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(CharacterLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, CharacterLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ClassInstanceCreation node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ClassInstanceCreation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(CompilationUnit node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, CompilationUnit.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ConditionalExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ConditionalExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ConstructorInvocation node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ConstructorInvocation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ContinueStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ContinueStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(DoStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, DoStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(EmptyStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, EmptyStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(EnhancedForStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, EnhancedForStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(EnumConstantDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, EnumConstantDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(EnumDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, EnumDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ExpressionStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ExpressionStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(FieldAccess node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, FieldAccess.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(FieldDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, FieldDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ForStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ForStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(IfStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, IfStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ImportDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ImportDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(InfixExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, InfixExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(Initializer node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, Initializer.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(InstanceofExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, InstanceofExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(Javadoc node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, Javadoc.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(LabeledStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, LabeledStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(LineComment node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, LineComment.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(MarkerAnnotation node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, MarkerAnnotation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(MemberRef node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, MemberRef.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(MemberValuePair node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, MemberValuePair.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(MethodDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, MethodDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(MethodInvocation node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, MethodInvocation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(MethodRef node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, MethodRef.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(MethodRefParameter node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, MethodRefParameter.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(Modifier node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, Modifier.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(NormalAnnotation node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, NormalAnnotation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(NullLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, NullLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(NumberLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, NumberLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(PackageDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, PackageDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ParameterizedType node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ParameterizedType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ParenthesizedExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ParenthesizedExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(PostfixExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, PostfixExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(PrefixExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, PrefixExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(PrimitiveType node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, PrimitiveType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(QualifiedName node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, QualifiedName.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(QualifiedType node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, QualifiedType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ReturnStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ReturnStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(SimpleName node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, SimpleName.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(SimpleType node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, SimpleType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(SingleMemberAnnotation node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, SingleMemberAnnotation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(SingleVariableDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, SingleVariableDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(StringLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, StringLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(SuperConstructorInvocation node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, SuperConstructorInvocation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(SuperFieldAccess node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, SuperFieldAccess.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(SuperMethodInvocation node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, SuperMethodInvocation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(SwitchCase node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, SwitchCase.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(SwitchStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, SwitchStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(SynchronizedStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, SynchronizedStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(TagElement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, TagElement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(TextElement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, TextElement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ThisExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ThisExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(ThrowStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, ThrowStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(TryStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, TryStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(TypeDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, TypeDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(TypeDeclarationStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, TypeDeclarationStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(TypeLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, TypeLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(TypeParameter node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, TypeParameter.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(UnionType node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, UnionType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(VariableDeclarationExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, VariableDeclarationExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(VariableDeclarationFragment node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, VariableDeclarationFragment.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(VariableDeclarationStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, VariableDeclarationStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(WhileStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, WhileStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void endVisit(WildcardType node) {
        final List<ASTVisitor> visitorList = getVisitors(endVisitorsMap, WildcardType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.endVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void postVisit(ASTNode node) {
        for (Iterator<ASTVisitor> iter = postVisitors.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.postVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void preVisit(ASTNode node) {
        for (Iterator<ASTVisitor> iter = preVisitors.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                v.preVisit(node);
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean preVisit2(ASTNode node) {
        for (Iterator<ASTVisitor> iter = preVisitors2.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!v.preVisit2(node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(AnnotationTypeDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, AnnotationTypeDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(AnnotationTypeMemberDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, AnnotationTypeMemberDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(AnonymousClassDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, AnonymousClassDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ArrayAccess node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ArrayAccess.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ArrayCreation node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ArrayCreation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ArrayInitializer node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ArrayInitializer.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ArrayType node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ArrayType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(AssertStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, AssertStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Assignment node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, Assignment.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Block node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, Block.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(BlockComment node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, BlockComment.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(BooleanLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, BooleanLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(BreakStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, BreakStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(CastExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, CastExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(CatchClause node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, CatchClause.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(CharacterLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, CharacterLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ClassInstanceCreation node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ClassInstanceCreation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(CompilationUnit node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, CompilationUnit.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ConditionalExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ConditionalExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ConstructorInvocation node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ConstructorInvocation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ContinueStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ContinueStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(DoStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, DoStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(EmptyStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, EmptyStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(EnhancedForStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, EnhancedForStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(EnumConstantDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, EnumConstantDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(EnumDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, EnumDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ExpressionStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ExpressionStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(FieldAccess node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, FieldAccess.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(FieldDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, FieldDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ForStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ForStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(IfStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, IfStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ImportDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ImportDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(InfixExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, InfixExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Initializer node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, Initializer.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(InstanceofExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, InstanceofExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Javadoc node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, Javadoc.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(LabeledStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, LabeledStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(LineComment node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, LineComment.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MarkerAnnotation node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, MarkerAnnotation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MemberRef node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, MemberRef.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MemberValuePair node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, MemberValuePair.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, MethodDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodInvocation node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, MethodInvocation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodRef node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, MethodRef.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodRefParameter node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, MethodRefParameter.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Modifier node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, Modifier.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(NormalAnnotation node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, NormalAnnotation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(NullLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, NullLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(NumberLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, NumberLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(PackageDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, PackageDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ParameterizedType node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ParameterizedType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ParenthesizedExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ParenthesizedExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(PostfixExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, PostfixExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(PrefixExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, PrefixExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(PrimitiveType node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, PrimitiveType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(QualifiedName node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, QualifiedName.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(QualifiedType node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, QualifiedType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ReturnStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ReturnStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SimpleName node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, SimpleName.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SimpleType node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, SimpleType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SingleMemberAnnotation node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, SingleMemberAnnotation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SingleVariableDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, SingleVariableDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(StringLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, StringLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SuperConstructorInvocation node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, SuperConstructorInvocation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SuperFieldAccess node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, SuperFieldAccess.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SuperMethodInvocation node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, SuperMethodInvocation.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SwitchCase node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, SwitchCase.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SwitchStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, SwitchStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(SynchronizedStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, SynchronizedStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TagElement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, TagElement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TextElement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, TextElement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ThisExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ThisExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ThrowStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, ThrowStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TryStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, TryStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TypeDeclaration node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, TypeDeclaration.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TypeDeclarationStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, TypeDeclarationStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TypeLiteral node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, TypeLiteral.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TypeParameter node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, TypeParameter.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(UnionType node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, UnionType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(VariableDeclarationExpression node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, VariableDeclarationExpression.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(VariableDeclarationFragment node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, VariableDeclarationFragment.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(VariableDeclarationStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, VariableDeclarationStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(WhileStatement node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, WhileStatement.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(WildcardType node) {
        final List<ASTVisitor> visitorList = getVisitors(visitorsMap, WildcardType.class);
        for (Iterator<ASTVisitor> iter = visitorList.iterator(); iter.hasNext();) {
            final ASTVisitor v = iter.next();
            try {
                if (!continueVisiting(v.visit(node), v, node)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                logFaultyVisitor(v, node, e);
                iter.remove();
            }
        }
        return VISIT_SUBTREE;
    }

}
