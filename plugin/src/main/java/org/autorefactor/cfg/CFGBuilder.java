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
package org.autorefactor.cfg;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.JavaProjectOptions;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.UnhandledException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
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
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.MemberRef;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.MethodRef;
import org.eclipse.jdt.core.dom.MethodRefParameter;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
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
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
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
import org.eclipse.jdt.core.dom.Type;
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

import static org.autorefactor.cfg.ASTPrintHelper.*;
import static org.autorefactor.cfg.CFGEdgeBuilder.*;
import static org.autorefactor.cfg.VariableAccess.*;
import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

/**
 * Builds a CFG.
 * <p>
 * Look at {@link #buildCFG(IfStatement, LivenessState, ThrowerBlocks)} for a
 * javadoc for all the buildCFG() methods.
 * <p>
 * TODO JNR detect dead code by looking for empty live blocks list when visiting a node
 * + looking at if / while / etc. conditions and see if they resolve to a constant
 */
public class CFGBuilder {

    private static final class LivenessState {

        /**
         * The currently live CFGBasicBlock. It can be null which means that further
         * analysis will first have to create a live CFGBasicBlock.
         */
        private final CFGBasicBlock liveBasicBlock;
        /**
         * The edges that are live on entering or after finishing analyzing a
         * statement.
         */
        private final List<CFGEdgeBuilder> liveEdges = new LinkedList<CFGEdgeBuilder>();

        private LivenessState() {
            this.liveBasicBlock = null;
        }

        private LivenessState(CFGBasicBlock liveBasicBlock) {
            this.liveBasicBlock = liveBasicBlock;
        }

        private LivenessState(CFGBasicBlock liveBasicBlock, CFGEdgeBuilder liveEdge) {
            this.liveBasicBlock = liveBasicBlock;
            this.liveEdges.add(liveEdge);
        }

        private static LivenessState of(List<CFGEdgeBuilder> liveEdges) {
            LivenessState result = new LivenessState();
            result.addAll(liveEdges);
            return result;
        }

        private static LivenessState of(CFGEdgeBuilder liveEdge) {
            LivenessState result = new LivenessState();
            result.add(liveEdge);
            return result;
        }

        private LivenessState copyLiveBasicBlock() {
            return new LivenessState(liveBasicBlock);
        }

        private LivenessState copyLiveEdges() {
            return LivenessState.of(liveEdges);
        }

        private LivenessState nextStmtWillCreateNewBlock() {
            return copyLiveEdges();
        }

        private LivenessState nextStmtsAreDeadCode() {
            return copyLiveEdges();
        }

        private boolean requireNewBlock() {
            return liveBasicBlock == null;
        }

        private void add(CFGEdgeBuilder edge) {
            liveEdges.add(edge);
        }

        private void addAll(Collection<CFGEdgeBuilder> edges) {
            liveEdges.addAll(edges);
        }

        private void addAll(LivenessState state) {
            liveEdges.addAll(state.liveEdges);
        }

        /** {@inheritDoc} */
        @Override
        public String toString() {
            return "LivenessState [liveBasicBlock=" + liveBasicBlock
                    + ", liveEdges=" + liveEdges + "]";
        }
    }

    private static final Pattern NEWLINE = Pattern.compile("\r\n|\r|\n");

    private final String source;
    private final int tabSize;
    /**
     * Edges to be built after visiting the statement used as the key.
     * <p>
     * After a statement is visited, code checks whether there are edges to
     * build and creates them.
     * </p>
     * <p>
     * This is only useful when labels are used with break or continue
     * statements which can send control flow back to any parent statement.
     * </p>
     */
    private final Map<Statement, Map<CFGEdgeBuilder, Boolean>> edgesToBuild =
            new HashMap<Statement, Map<CFGEdgeBuilder, Boolean>>();
    /** The exit block for the CFG being built. */
    private CFGBasicBlock exitBlock;

    /**
     * Cache of "manually" resolved type bindings.
     * <p>
     * Cannot be made static because for {@link ITypeBinding#equals(Object)} to
     * work, all type bindings must have been loaded from the same
     * CompilationUnit.
     */
    private final Map<String, ITypeBinding> typeBindingsCache = new HashMap<String, ITypeBinding>();

    /**
     * Builds an instance of this class.
     *
     * @param source the java source code to work from
     * @param options the Java project options used to compile the project
     */
    public CFGBuilder(String source, JavaProjectOptions options) {
        this.source = source;
        this.tabSize = options.getTabSize();
    }

    /**
     * @return whether the current variable access can throw an exception.
     */
    @SuppressWarnings("unchecked")
    private boolean addVariableAccess(CFGBasicBlock basicBlock, Expression node,
            int flags, ThrowerBlocks throwers) {
        if (node == null) {
            return false;
        }
        switch (node.getNodeType()) {
        case ARRAY_ACCESS:
            ArrayAccess aa = (ArrayAccess) node;
            addVariableAccess(basicBlock, aa.getArray(), flags, throwers);
            addVariableAccess(basicBlock, aa.getIndex(), flags, throwers);
            throwers.addThrow(aa, newException(node, "java.lang.ArrayIndexOutOfBoundsException"));
            return true;
        case ARRAY_CREATION:
            ArrayCreation ac = (ArrayCreation) node;
            boolean acMightThrow1 = addVariableAccess(basicBlock, ac.getInitializer(), flags, throwers);
            boolean acMightThrow2 = addVariableAccesses(basicBlock, ac.dimensions(), flags, throwers);
            return acMightThrow1 || acMightThrow2;
        case ARRAY_INITIALIZER:
            ArrayInitializer ai = (ArrayInitializer) node;
            return addVariableAccesses(basicBlock, ai.expressions(), flags, throwers);
        case ASSIGNMENT:
            Assignment a = (Assignment) node;
            boolean aMightThrow1 = addVariableAccess(basicBlock, a.getLeftHandSide(), WRITE, throwers);
            boolean aMightThrow2 = addVariableAccess(basicBlock, a.getRightHandSide(), READ, throwers);
            return aMightThrow1 || aMightThrow2;
        case BOOLEAN_LITERAL:
        case CHARACTER_LITERAL:
        case NULL_LITERAL:
        case NUMBER_LITERAL:
        case STRING_LITERAL:
        case TYPE_LITERAL:
            // nothing to do
            return false;
        case CAST_EXPRESSION:
            CastExpression cae = (CastExpression) node;
            return addVariableAccess(basicBlock, cae.getExpression(), flags, throwers);
        case CLASS_INSTANCE_CREATION:
            ClassInstanceCreation cic = (ClassInstanceCreation) node;
            addVariableAccess(basicBlock, cic.getExpression(), flags, throwers);
            addVariableAccesses(basicBlock, cic.arguments(), flags, throwers);
            IMethodBinding cicBinding = cic.resolveConstructorBinding();
            if (cicBinding != null) {
                ITypeBinding[] declaredThrows = cicBinding.getExceptionTypes();
                throwers.addThrow(cic, declaredThrows);
                return declaredThrows.length > 0;
            }
            return false;
        case CONDITIONAL_EXPRESSION:
            ConditionalExpression coe = (ConditionalExpression) node;
            boolean mightThrow1 = addVariableAccess(basicBlock, coe.getExpression(), flags, throwers);
            boolean mightThrow2 = addVariableAccess(basicBlock, coe.getThenExpression(), flags, throwers);
            boolean mightThrow3 = addVariableAccess(basicBlock, coe.getElseExpression(), flags, throwers);
            return mightThrow1 || mightThrow2 || mightThrow3;
        case FIELD_ACCESS:
            FieldAccess fa = (FieldAccess) node;
            boolean mightThrow = addVariableAccess(basicBlock, fa.getExpression(), flags, throwers);
            basicBlock.addVariableAccess(new VariableAccess(fa, flags));
            if (is(flags, READ)) {
                throwers.addThrow(fa, newException(node, "java.lang.NullPointerException"));
                mightThrow = true;
            }
            return mightThrow;
        case INFIX_EXPRESSION:
            InfixExpression ie = (InfixExpression) node;
            boolean ieMightThrow1 = addVariableAccess(basicBlock, ie.getLeftOperand(), flags, throwers);
            boolean ieMightThrow2 = addVariableAccess(basicBlock, ie.getRightOperand(), flags, throwers);
            return ieMightThrow1 || ieMightThrow2;
        case INSTANCEOF_EXPRESSION:
            InstanceofExpression ioe = (InstanceofExpression) node;
            return addVariableAccess(basicBlock, ioe.getLeftOperand(), flags, throwers);
        case METHOD_INVOCATION:
            MethodInvocation mi = (MethodInvocation) node;
            addVariableAccess(basicBlock, mi.getExpression(), flags, throwers);
            addVariableAccesses(basicBlock, mi.arguments(), flags, throwers);
            IMethodBinding methodBinding = mi.resolveMethodBinding();
            if (methodBinding != null) {
                ITypeBinding[] declaredThrows = methodBinding.getExceptionTypes();
                throwers.addThrow(mi, declaredThrows);
                return declaredThrows.length > 0;
            }
            return false;
        case SIMPLE_NAME:
            SimpleName sn = (SimpleName) node;
            basicBlock.addVariableAccess(new VariableAccess(sn, flags));
            if (is(flags, READ)) {
                throwers.addThrow(sn, newException(node, "java.lang.NullPointerException"));
                return true;
            }
            return false;
        case QUALIFIED_NAME:
            QualifiedName qn = (QualifiedName) node;
            basicBlock.addVariableAccess(new VariableAccess(qn, flags));
            throwers.addThrow(qn, newException(node, "java.lang.NullPointerException"));
            return true;
        case PARENTHESIZED_EXPRESSION:
            ParenthesizedExpression pe = (ParenthesizedExpression) node;
            return addVariableAccess(basicBlock, pe.getExpression(), flags, throwers);
        case POSTFIX_EXPRESSION:
            PostfixExpression poe = (PostfixExpression) node;
            return addVariableAccess(basicBlock, poe.getOperand(), flags, throwers);
        case PREFIX_EXPRESSION:
            PrefixExpression pre = (PrefixExpression) node;
            return addVariableAccess(basicBlock, pre.getOperand(), flags, throwers);
        case SUPER_FIELD_ACCESS:
            SuperFieldAccess sfa = (SuperFieldAccess) node;
            boolean sfaMightThrow1 = addVariableAccess(basicBlock, sfa.getQualifier(), flags, throwers);
            boolean sfaMightThrow2 = addVariableAccess(basicBlock, sfa.getName(), flags, throwers);
            return sfaMightThrow1 || sfaMightThrow2;
        case SUPER_METHOD_INVOCATION:
            SuperMethodInvocation smi = (SuperMethodInvocation) node;
            addVariableAccess(basicBlock, smi.getQualifier(), flags, throwers);
            addVariableAccess(basicBlock, smi.getName(), flags, throwers);
            IMethodBinding sMethodBinding = smi.resolveMethodBinding();
            if (sMethodBinding != null) {
                ITypeBinding[] declaredThrows = sMethodBinding.getExceptionTypes();
                throwers.addThrow(smi, declaredThrows);
                return declaredThrows.length > 0;
            }
            return false;
        case THIS_EXPRESSION:
            ThisExpression te = (ThisExpression) node;
            // TODO JNR remember use of "this" here
            return addVariableAccess(basicBlock, te.getQualifier(), flags, throwers);
        case VARIABLE_DECLARATION_EXPRESSION:
            return addDeclarations(basicBlock, (VariableDeclarationExpression) node, throwers);
        default:
            throw new NotImplementedException(node);
        }
    }

    private boolean is(int flags, int flag) {
        return (flags & flag) == flag;
    }

    private ITypeBinding newException(Expression node, String fullyQualifiedName) {
        ITypeBinding typeBinding = typeBindingsCache.get(fullyQualifiedName);
        if (typeBinding == null) {
            typeBinding = resolveWellKnownType(node, fullyQualifiedName);
            typeBindingsCache.put(typeBinding.getQualifiedName(), typeBinding);
        }
        return typeBinding;
    }

    /**
     * FIXME Horribly brittle hack that uses reflection to resolve type bindings.
     * <p>
     * But how could I do otherwise?
     * <p>
     *
     * @see org.eclipse.jdt.core.dom.DefaultBindingResolver#resolveWellKnownType(String)
     */
    private ITypeBinding resolveWellKnownType(Expression node, String fullyQualifiedName) {
        try {
            final ITypeBinding typeBinding = node.resolveTypeBinding();

            final Field f1 = typeBinding.getClass().getDeclaredField("resolver");
            f1.setAccessible(true);
            Object bindingResolver = f1.get(typeBinding);

            final Field f2 = bindingResolver.getClass().getDeclaredField("scope");
            f2.setAccessible(true);
            Object compilationUnitScope = f2.get(bindingResolver);

            final Method m2 = compilationUnitScope.getClass().getSuperclass()
                    .getDeclaredMethod("getType", char[][].class, int.class);
            m2.setAccessible(true);
            final char[][] simpleNamesArray = toSimpleNamesArray(fullyQualifiedName);
            final Object internalTypeBinding =
                    m2.invoke(compilationUnitScope, simpleNamesArray, 3);

            final Method m1 = bindingResolver.getClass().getDeclaredMethod("getTypeBinding",
                    internalTypeBinding.getClass().getSuperclass().getSuperclass());
            m1.setAccessible(true);
            return (ITypeBinding) m1.invoke(bindingResolver, internalTypeBinding);
        } catch (Exception e) {
            throw new UnhandledException(node, e);
        }
    }

    private char[][] toSimpleNamesArray(String fullyQualifiedName) {
        final String[] simpleNames = fullyQualifiedName.split("\\.");
        final char[][] result = new char[simpleNames.length][];
        for (int i = 0; i < simpleNames.length; i++) {
            result[i] = simpleNames[i].toCharArray();
        }
        return result;
    }

    /**
     * @return whether the current variable accesses can throw an exception.
     */
    private boolean addVariableAccesses(CFGBasicBlock basicBlock,
            List<Expression> expressions, int flags, ThrowerBlocks throwers) {
        boolean mightThrow = false;
        for (Expression expr : expressions) {
            if (addVariableAccess(basicBlock, expr, flags, throwers)) {
                mightThrow = true;
            }
        }
        return mightThrow;
    }

    private boolean addDeclarations(CFGBasicBlock basicBlock,
            List<VariableDeclarationFragment> fragments, Type type, ThrowerBlocks throwers) {
        boolean mightThrow = false;
        for (VariableDeclarationFragment vdf : fragments) {
            if (addDeclaration(basicBlock, vdf, type, throwers)) {
                mightThrow = true;
            }
        }
        return mightThrow;
    }

    private boolean addDeclaration(final CFGBasicBlock basicBlock,
            VariableDeclarationFragment vdf, Type type, ThrowerBlocks throwers) {
        final int accessType = vdf.getInitializer() == null ? DECL_UNINIT
                : DECL_INIT | WRITE;
        basicBlock.addVariableAccess(new VariableAccess(vdf, vdf.getName(),
                type, accessType));
        return addVariableAccess(basicBlock, vdf.getInitializer(), READ, throwers);
    }

    private boolean addDeclarations(CFGBasicBlock basicBlock,
            final VariableDeclarationExpression vde, ThrowerBlocks throwers) {
        return addDeclarations(basicBlock, fragments(vde), vde.getType(), throwers);
    }

    private void addDeclarations(CFGBasicBlock basicBlock,
            List<SingleVariableDeclaration> varDecls) {
        for (SingleVariableDeclaration varDecl : varDecls) {
            addDeclaration(basicBlock, varDecl);
        }
    }

    private void addDeclaration(final CFGBasicBlock basicBlock,
            final SingleVariableDeclaration varDecl) {
        addDeclaration(basicBlock, varDecl, DECL_INIT);
    }

    private void addDeclaration(CFGBasicBlock basicBlock,
            SingleVariableDeclaration varDecl, int flags) {
        basicBlock.addVariableAccess(new VariableAccess(varDecl, varDecl
                .getName(), varDecl.getType(), flags));
    }

    private LivenessState buildCFG(Statement node, LivenessState state, ThrowerBlocks throwers) {
        if (node == null) {
            return state.nextStmtWillCreateNewBlock();
        }
        try {
            final Method m = getClass().getMethod(
                    "buildCFG", node.getClass(), LivenessState.class, ThrowerBlocks.class);
            return (LivenessState) m.invoke(this, node, state, throwers);
        } catch (Exception e) {
            throw new UnhandledException(node, e);
        }
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(QualifiedName node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(PrimitiveType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(QualifiedType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(PrefixExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(PostfixExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(ParenthesizedExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(SingleVariableDeclaration node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(SimpleType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(SimpleName node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(ReturnStatement node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        if (node.getExpression() != null) {
            addVariableAccess(basicBlock, node.getExpression(), READ, throwers);
        }
        buildEdge(basicBlock, this.exitBlock);
        return state.nextStmtsAreDeadCode();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(Modifier node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(MethodInvocation node) {
        // TODO JNR add variable access to "this"
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @return the entry block to the CFG of this method declaration
     */
    public CFGBasicBlock buildCFG(MethodDeclaration node) {
        final CFGBasicBlock entryBlock = newEntryBlock(node);
        this.exitBlock = newExitBlock(node);

        addDeclarations(entryBlock, parameters(node));

        try {
            final ThrowerBlocks throwers = new ThrowerBlocks();
            final CFGEdgeBuilder liveEdge = new CFGEdgeBuilder(entryBlock);
            final LivenessState liveAfterBody = buildCFG(node.getBody(), LivenessState.of(liveEdge), throwers);
            if (!liveAfterBody.liveEdges.isEmpty()) {
                if (node.getReturnType2() == null
                        || node.getReturnType2().resolveBinding() == null // added for unit tests
                        || "void".equals(node.getReturnType2().resolveBinding().getName())) {
                    buildEdges(liveAfterBody, exitBlock);
                } else {
                    throw new IllegalStateException(node, "Did not expect to find any edges to build "
                        + "for a constructor or a non void method return type.");
                }
            }
            if (!this.edgesToBuild.isEmpty()) {
                throw new IllegalStateException(node,
                        "At this point, there should not be any edges left to build. Left edges: " + this.edgesToBuild);
            }
            List<CFGBasicBlock> throwingBlocks = throwers.selectBlocksThrowing(null);
            if (!throwingBlocks.isEmpty()) {
                for (CFGBasicBlock throwingBlock : throwingBlocks) {
                    // TODO JNR
                }
            }
            List<CFGEdgeBuilder> throwingEdges = throwers.selectEdgesThrowing(null);
            if (!throwingEdges.isEmpty()) {
                for (CFGEdgeBuilder throwingEdge : throwingEdges) {
                    // TODO JNR
                }
            }
            return entryBlock;
        } finally {
            this.exitBlock = null;
        }
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(MethodRefParameter node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(MethodRef node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(MemberValuePair node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(ParameterizedType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(NumberLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(NullLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(UnionType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(TypeParameter node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(TypeLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(TypeDeclarationStatement node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @return the list of basic blocks representing CFGs for each method in this type declaration
     */
    public List<CFGBasicBlock> buildCFG(TypeDeclaration node) {
        if (!node.isInterface()) {
            List<CFGBasicBlock> results = new LinkedList<CFGBasicBlock>();
            for (FieldDeclaration fieldDecl : node.getFields()) {
                buildCFG(fieldDecl);
            }
            for (MethodDeclaration methodDecl : node.getMethods()) {
                results.add(buildCFG(methodDecl));
            }
            for (TypeDeclaration typeDeclaration : node.getTypes()) {
                buildCFG(typeDeclaration);
            }
            // for (BodyDeclaration bodyDeclaration : (List<BodyDeclaration>)
            // node.bodyDeclarations()) {
            // buildCFG(bodyDeclaration);
            // }
            return results;
        }
        return Collections.emptyList();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(TryStatement node, LivenessState state, ThrowerBlocks throwers) {
        final ThrowerBlocks localThrowers = new ThrowerBlocks();

        final LivenessState liveAfterTry = buildCFG(node.getBody(), state, localThrowers);
        final LivenessState liveAfterCatchClauses = new LivenessState();

        final Set<ITypeBinding> caughtExceptions = new HashSet<ITypeBinding>();
        for (CatchClause catchClause : catchClauses(node)) {
            final LivenessState catchState = new LivenessState();
            CFGBasicBlock catchBasicBlock = getCFGBasicBlock(catchClause, catchState);
            final SingleVariableDeclaration exceptionDecl = catchClause.getException();
            addDeclaration(catchBasicBlock, exceptionDecl, DECL_INIT);

            final ITypeBinding caughtException = exceptionDecl.getType().resolveBinding();
            caughtExceptions.add(caughtException);

            final List<CFGBasicBlock> throwingBlocksInTry = localThrowers.selectBlocksThrowing(caughtException);
            if (throwingBlocksInTry.isEmpty()) {
                // TODO JNR dead code found!!
            }
            final List<CFGEdgeBuilder> liveBeforeCatchClause = new LinkedList<CFGEdgeBuilder>();
            for (CFGBasicBlock throwingBlockInTry : throwingBlocksInTry) {
                // TODO JNR if a Statement throws an exception, it must break the current basicBlock
                // TODO JNR how to specify this edge is due to an exception?
                liveBeforeCatchClause.add(new CFGEdgeBuilder(throwingBlockInTry));
            }

            final LivenessState liveAfterCatchClause = buildCFG(catchClause.getBody(), catchState, new ThrowerBlocks());
            liveAfterCatchClauses.addAll(liveAfterCatchClause);
        }

        // TODO JNR move uncaught exceptions from localThrowers to throwers
        final Map<CFGBasicBlock, Set<ITypeBinding>> throwUncaughtExceptions =
                localThrowers.selectBlocksThrowingOtherThan(caughtExceptions);
        for (Entry<CFGBasicBlock, Set<ITypeBinding>> throwing : throwUncaughtExceptions.entrySet()) {
            final CFGEdgeBuilder uncaughtExceptionEdge = new CFGEdgeBuilder(throwing.getKey(), true);
            liveAfterCatchClauses.add(uncaughtExceptionEdge);
            throwers.addThrow(uncaughtExceptionEdge, throwing.getValue());
        }

        if (node.getFinally() != null) {
            final LivenessState liveBeforeFinally = new LivenessState();
            liveBeforeFinally.addAll(liveAfterTry);
            liveBeforeFinally.addAll(liveAfterCatchClauses);
            final LivenessState liveAfterFinally = buildCFG(node.getFinally(), liveBeforeFinally, throwers);
            return liveAfterFinally;
        } else {
            return liveAfterCatchClauses.copyLiveBasicBlock();
        }
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(WildcardType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(WhileStatement node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock conditionBlock = getCFGBasicBlock(node.getExpression(), state.nextStmtWillCreateNewBlock());
        addVariableAccess(conditionBlock, node.getExpression(), READ, throwers);

        final CFGEdgeBuilder liveEdge = new CFGEdgeBuilder(node.getExpression(), true, conditionBlock);
        final LivenessState liveAfterStmt = buildCFG(node.getBody(), LivenessState.of(liveEdge), throwers);
        liveAfterStmt.add(new CFGEdgeBuilder(node.getExpression(), false, conditionBlock));
        buildEdgesAfterBranchableStmt(node, liveAfterStmt, conditionBlock);
        return liveAfterStmt.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(VariableDeclarationFragment node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(VariableDeclarationStatement node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        addDeclarations(basicBlock, fragments(node), node.getType(), throwers);
        return getInBlockStmtResult(state, basicBlock);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(VariableDeclarationExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(SwitchStatement node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        final LivenessState liveBeforeBody = new LivenessState(basicBlock, new CFGEdgeBuilder(basicBlock));
        final LivenessState liveAfterBody = buildCFG(statements(node), liveBeforeBody, throwers);
        liveAfterBody.add(new CFGEdgeBuilder(basicBlock));

        buildEdgesAfterBranchableStmt(node, liveAfterBody, basicBlock);
        return liveAfterBody.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param switchConditionBasicBlock the basic block for the switch condition
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(SwitchCase node, CFGBasicBlock switchConditionBasicBlock,
            LivenessState state, ThrowerBlocks throwers) {
        // the current live blocks will be empty if there was a break,
        // or populated in case of fall-through.
        addVariableAccess(switchConditionBasicBlock, node.getExpression(), READ, throwers);
        // add an edge going from the condition of the switch
        // (state.liveBasicBlock is the condition of the switch)
        state.add(new CFGEdgeBuilder(node.getExpression(), true, switchConditionBasicBlock));
        return state.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(SuperMethodInvocation node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(SuperFieldAccess node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(SuperConstructorInvocation node, LivenessState state,
            ThrowerBlocks throwers) {
        final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        addVariableAccesses(basicBlock, arguments(node), READ, throwers);
        return getInBlockStmtResult(state, basicBlock);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(StringLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(ThrowStatement node, LivenessState state, ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        final Expression throwingExpr = node.getExpression();
        addVariableAccess(state.liveBasicBlock, throwingExpr, READ, throwers);
        throwers.addThrow(basicBlock, throwingExpr.resolveTypeBinding());
        return state.nextStmtsAreDeadCode();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(ThisExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(TextElement node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(TagElement node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(SynchronizedStatement node, LivenessState state, ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock = getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());
        addVariableAccess(basicBlock, node.getExpression(), READ, throwers);
        CFGEdgeBuilder liveEdge = new CFGEdgeBuilder(basicBlock);
        LivenessState result = buildCFG(node.getBody(), LivenessState.of(liveEdge), throwers);
        return result.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(CatchClause node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(CastExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(BreakStatement node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        final Statement targetStmt;
        if (node.getLabel() != null) {
            targetStmt = findLabeledParentStmt(node);
        } else {
            targetStmt = findBreakableParentStmt(node);
        }
        addEdgeToBuild(targetStmt, new CFGEdgeBuilder(basicBlock), true);
        return state.copyLiveBasicBlock();
    }

    private Statement findLabeledParentStmt(ASTNode node) {
        ASTNode n = node;
        while (n != null && n.getNodeType() != LABELED_STATEMENT) {
            n = n.getParent();
        }
        if (n != null) {
            return ((LabeledStatement) n).getBody();
        }
        return null;
    }

    private Statement findBreakableParentStmt(ASTNode node) {
        ASTNode n = node;
        while (n != null && !ASTHelper.isBreakable(n)) {
            n = n.getParent();
        }
        if (n != null) {
            return (Statement) n;
        }
        return null;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(BooleanLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(ConstructorInvocation node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        addVariableAccesses(basicBlock, arguments(node), READ, throwers);
        return getInBlockStmtResult(state, basicBlock);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(ConditionalExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @return the list of basic blocks representing CFGs for each method in this compilation unit
     */
    public List<CFGBasicBlock> buildCFG(CompilationUnit node) {
        List<CFGBasicBlock> results = new LinkedList<CFGBasicBlock>();
        for (AbstractTypeDeclaration decl : (List<AbstractTypeDeclaration>) node.types()) {
            if (decl.getNodeType() == TYPE_DECLARATION) {
                results.addAll(buildCFG((TypeDeclaration) decl));
            } else {
                throw new NotImplementedException(node);
            }
        }
        return results;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(ClassInstanceCreation node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(CharacterLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(ArrayCreation node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(ArrayAccess node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(AnonymousClassDeclaration node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(Block node, LivenessState state, ThrowerBlocks throwers) {
        LivenessState liveState = state;
        try {
            liveState = buildCFG(statements(node), state, throwers);
        } finally {
            moveAllEdgesToBuild(node, liveState);
        }
        return liveState.nextStmtWillCreateNewBlock();
    }

    private LivenessState buildCFG(List<Statement> stmts, final LivenessState startState, ThrowerBlocks throwers) {
        LivenessState liveState = startState;
        for (Statement stmt : stmts) {
            switch (stmt.getNodeType()) {
            case ASSERT_STATEMENT:
                liveState = buildCFG((AssertStatement) stmt, liveState, throwers);
                break;
            case BLOCK:
                liveState = buildCFG((Block) stmt, liveState, throwers);
                break;
            case BREAK_STATEMENT:
                liveState = buildCFG((BreakStatement) stmt, liveState, throwers);
                break;
            case CONSTRUCTOR_INVOCATION:
                liveState = buildCFG(stmt, liveState, throwers);
                break;
            case CONTINUE_STATEMENT:
                liveState = buildCFG((ContinueStatement) stmt, liveState, throwers);
                break;
            case DO_STATEMENT:
                liveState = buildCFG((DoStatement) stmt, liveState, throwers);
                break;
            case EMPTY_STATEMENT:
                liveState = buildCFG((EmptyStatement) stmt, liveState, throwers);
                break;
            case ENHANCED_FOR_STATEMENT:
                liveState = buildCFG((EnhancedForStatement) stmt, liveState, throwers);
                break;
            case EXPRESSION_STATEMENT:
                liveState = buildCFG((ExpressionStatement) stmt, liveState, throwers);
                break;
            case FOR_STATEMENT:
                liveState = buildCFG((ForStatement) stmt, liveState, throwers);
                break;
            case IF_STATEMENT:
                liveState = buildCFG((IfStatement) stmt, liveState, throwers);
                break;
            case LABELED_STATEMENT:
                liveState = buildCFG((LabeledStatement) stmt, liveState, throwers);
                break;
            case RETURN_STATEMENT:
                liveState = buildCFG((ReturnStatement) stmt, liveState, throwers);
                break;
            case SUPER_CONSTRUCTOR_INVOCATION:
                liveState = buildCFG(stmt, liveState, throwers);
                break;
            case SWITCH_CASE:
                // Here, use startState.liveBasicBlock to build an edge
                // from the switch condition to the case statement
                liveState = buildCFG((SwitchCase) stmt, startState.liveBasicBlock, liveState, throwers);
                break;
            case SWITCH_STATEMENT:
                liveState = buildCFG((SwitchStatement) stmt, liveState, throwers);
                break;
            case SYNCHRONIZED_STATEMENT:
                liveState = buildCFG((SynchronizedStatement) stmt, liveState, throwers);
                break;
            case THROW_STATEMENT:
                liveState = buildCFG((ThrowStatement) stmt, liveState, throwers);
                break;
            case TRY_STATEMENT:
                liveState = buildCFG((TryStatement) stmt, liveState, throwers);
                // break;case TYPE_DECLARATION_STATEMENT:
                // buildCFG((TypeDeclarationStatement) stmt, liveState, throwers);
                break;
            case VARIABLE_DECLARATION_STATEMENT:
                liveState = buildCFG((VariableDeclarationStatement) stmt, liveState, throwers);
                break;
            case WHILE_STATEMENT:
                liveState = buildCFG((WhileStatement) stmt, liveState, throwers);
                break;
            default:
                throw new NotImplementedException(stmt);
            }
        }
        return liveState;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(Assignment node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(AssertStatement node, LivenessState state, ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        addVariableAccess(basicBlock, node.getExpression(), READ, throwers);
        addVariableAccess(basicBlock, node.getMessage(), READ, throwers);
        return getInBlockStmtResult(state, basicBlock);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(ArrayType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(ArrayInitializer node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(Initializer node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(InstanceofExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(InfixExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the passed in statement.
     *
     * @param node the statement for which to build a CFG
     * @param state the liveness state before the current statement.
     *          It contains: the live edges before the current statement,
     *          the live basic block to which the current statement might be added.
     *          If null, then the new basic block must be created for the current statement.
     * @param throwers the thrower blocks information
     * @return the new live state after the current statement
     */
    public LivenessState buildCFG(IfStatement node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock exprBlock = getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock(), true);
        try {
            addVariableAccess(exprBlock, node.getExpression(), READ, throwers);

            final LivenessState result = new LivenessState();
            CFGEdgeBuilder thenEdge = new CFGEdgeBuilder(node.getExpression(), true, exprBlock);
            result.addAll(buildCFG(node.getThenStatement(), LivenessState.of(thenEdge), throwers));

            final Statement elseStmt = node.getElseStatement();
            CFGEdgeBuilder elseEdge = new CFGEdgeBuilder(node.getExpression(), false, exprBlock);
            if (elseStmt != null) {
                result.addAll(buildCFG(elseStmt, LivenessState.of(elseEdge), throwers));
            } else {
                result.add(elseEdge);
            }
            return result.nextStmtWillCreateNewBlock();
        } finally {
            moveAllEdgesToBuild(node, state);
        }
    }

    private void addEdgeToBuild(final Statement node, CFGEdgeBuilder builder, boolean isBreakStmt) {
        if (builder != null) {
            Map<CFGEdgeBuilder, Boolean> builders = this.edgesToBuild.get(node);
            if (builders == null) {
                builders = new HashMap<CFGEdgeBuilder, Boolean>();
                this.edgesToBuild.put(node, builders);
            }
            builders.put(builder, isBreakStmt);
        }
    }

    private void moveAllEdgesToBuild(Statement node, LivenessState state) {
        final Map<CFGEdgeBuilder, Boolean> toBuild = this.edgesToBuild.remove(node);
        if (toBuild != null) {
            state.addAll(toBuild.keySet());
        }
    }

    private void buildEdges(final LivenessState toBuild, final CFGBasicBlock targetBlock) {
        if (isNotEmpty(toBuild.liveEdges)) {
            for (CFGEdgeBuilder builder : toBuild.liveEdges) {
                builder.withTarget(targetBlock).build();
            }
            toBuild.liveEdges.clear();
        }
    }

    private void buildEdgesAfterBranchableStmt(Statement node,
            final LivenessState liveAfterBranchableStmt, final CFGBasicBlock whereToBranchBlock) {
        final Map<CFGEdgeBuilder, Boolean> toBuild = this.edgesToBuild.remove(node);
        if (isNotEmpty(toBuild)) {
            Set<Entry<CFGEdgeBuilder, Boolean>> set = toBuild.entrySet();
            for (Iterator<Entry<CFGEdgeBuilder, Boolean>> iter = set.iterator(); iter.hasNext();) {
                Entry<CFGEdgeBuilder, Boolean> entry = iter.next();
                final CFGEdgeBuilder builder = entry.getKey();
                final boolean isBreakStmt = entry.getValue();
                if (isBreakStmt) {
                    liveAfterBranchableStmt.add(builder);
                } else { // this is a continue statement
                    builder.withTarget(whereToBranchBlock).build();
                    iter.remove();
                }
            }
        }
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(MemberRef node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(LabeledStatement node, LivenessState state, ThrowerBlocks throwers) {
        // does not count as an executable node, so do not get a basic block for it
        return buildCFG(node.getBody(), state, throwers);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(EnhancedForStatement node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());

        addDeclaration(basicBlock, node.getParameter(), DECL_INIT | WRITE);

        final LivenessState newLiveState = LivenessState.of(new CFGEdgeBuilder(basicBlock));
        final LivenessState liveAfterBody = buildCFG(node.getBody(), newLiveState, throwers);
        buildEdges(liveAfterBody, basicBlock);

        final LivenessState liveAfterStmt = LivenessState.of(new CFGEdgeBuilder(basicBlock));
        buildEdgesAfterBranchableStmt(node, liveAfterStmt, basicBlock);
        return liveAfterStmt.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(EmptyStatement node, LivenessState state, ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        return getInBlockStmtResult(state, basicBlock);
    }

    private LivenessState getInBlockStmtResult(LivenessState state, CFGBasicBlock basicBlock) {
        if (state.liveBasicBlock == null) {
            // new block was created for current node
            return new LivenessState(basicBlock, new CFGEdgeBuilder(basicBlock));
        }
        return state;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(DoStatement node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());
        final LivenessState newLiveState = new LivenessState(basicBlock, new CFGEdgeBuilder(basicBlock));
        final LivenessState liveAfterLoop = buildCFG(node.getBody(), newLiveState, throwers);
        CFGBasicBlock conditionBlock =
                getCFGBasicBlock(node.getExpression(), liveAfterLoop.nextStmtWillCreateNewBlock());
        addVariableAccess(conditionBlock, node.getExpression(), READ, throwers);

        buildEdge(node.getExpression(), true, conditionBlock, basicBlock);

        LivenessState liveAfterStmt = LivenessState.of(new CFGEdgeBuilder(node.getExpression(), false, conditionBlock));
        buildEdgesAfterBranchableStmt(node, liveAfterStmt, basicBlock);
        return liveAfterStmt;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(ContinueStatement node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        final Statement targetStmt;
        if (node.getLabel() != null) {
            targetStmt = findLabeledParentStmt(node);
        } else {
            targetStmt = findContinuableParentStmt(node);
        }
        addEdgeToBuild(targetStmt, new CFGEdgeBuilder(basicBlock), false);
        return state.copyLiveBasicBlock();
    }

    private Statement findContinuableParentStmt(ASTNode node) {
        ASTNode n = node;
        while (n != null && !ASTHelper.isLoop(n)) {
            n = n.getParent();
        }
        if (n != null) {
            return (Statement) n;
        }
        return null;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(ForStatement node, LivenessState state, ThrowerBlocks throwers) {
        final CFGBasicBlock initBlock = getCFGBasicBlock(initializers(node), state);
        final LivenessState initLiveBlock = LivenessState.of(new CFGEdgeBuilder(initBlock));
        final CFGBasicBlock exprBlock = getCFGBasicBlock(node.getExpression(), initLiveBlock, true);
        final CFGBasicBlock updatersBlock = getCFGBasicBlock(updaters(node), new LivenessState());
        buildEdge(updatersBlock, exprBlock);

        for (Expression expression : initializers(node)) {
            if (expression instanceof VariableDeclarationExpression) {
                addDeclarations(initBlock, (VariableDeclarationExpression) expression, throwers);
            }
        }
        addVariableAccess(exprBlock, node.getExpression(), READ, throwers);
        addVariableAccesses(updatersBlock, updaters(node), WRITE, throwers);

        CFGEdgeBuilder liveBlock = new CFGEdgeBuilder(node.getExpression(), true, exprBlock);
        final LivenessState liveAfterBody = buildCFG(node.getBody(), LivenessState.of(liveBlock), throwers);
        buildEdges(liveAfterBody, updatersBlock);

        final LivenessState liveAfterStmt = LivenessState.of(new CFGEdgeBuilder(
                node.getExpression(), false, exprBlock));
        buildEdgesAfterBranchableStmt(node, liveAfterStmt, updatersBlock);
        return liveAfterStmt.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(FieldDeclaration node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(FieldAccess node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @param state the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(ExpressionStatement node, LivenessState state, ThrowerBlocks throwers) {
        boolean isNewBlock = state.requireNewBlock();
        final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
        boolean mightThrow = addVariableAccess(basicBlock, node.getExpression(), READ, throwers);
        if (!isNewBlock && mightThrow) {
            final CFGBasicBlock currentBlock = getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());
            return new LivenessState(currentBlock, new CFGEdgeBuilder(currentBlock));
            // TODO JNR create a new CFGBasicBlock from here to catch / finally / exit
        }
        return getInBlockStmtResult(state, basicBlock);
    }

    private CFGBasicBlock getCFGBasicBlock(ASTNode node, LivenessState state) {
        return getCFGBasicBlock(node, state, false);
    }

    /**
     * Will create and return a new CFGBasicBlock for the passed in node, if the liveBasicBlock is null, otherwise
     * it will return the liveBasicBlock.
     *
     * @param node
     * @param state the liveness state the current statement will be added to.
     *        A null liveBasicBlock forces the creation of a new CFGBasicBlock.
     *        liveEdges are the edges that are live before getting the CFGBasicBlock
     * @param isDecision used for building the associated CFGEdge
     * @return
     */
    private CFGBasicBlock getCFGBasicBlock(ASTNode node, LivenessState state, boolean isDecision) {
        final Map<CFGEdgeBuilder, Boolean> toBuild = this.edgesToBuild.remove(node);
        if (isNotEmpty(toBuild)) {
            throw new IllegalStateException(node, "No edges to build should exist for node \"" + node
                + "\" before a CFGBasicBlock is created for it. Found the following edges to build " + toBuild);
        }
        if (!state.requireNewBlock()) {
            final CFGBasicBlock basicBlock = state.liveBasicBlock;
            // TODO JNR add nodes to the basicBlock they belong to
            // and adapt the CFGDotPrinter to display "..." after the first node
            // basicBlock.addNode(node);
            return basicBlock;
        }
        final LineAndColumn lineCol = getLineAndColumn(node);
        final CFGBasicBlock basicBlock = new CFGBasicBlock(node,
                getFileName(node), codeExcerpt(node), isDecision, lineCol);
        buildEdges(state, basicBlock);
        return basicBlock;
    }

    private CFGBasicBlock getCFGBasicBlock(List<Expression> expressions, LivenessState state) {
        if (isNotEmpty(expressions)) {
            final Expression firstExpr = expressions.get(0);
            final LineAndColumn lineCol = getLineAndColumn(firstExpr.getStartPosition());
            final CFGBasicBlock basicBlock = new CFGBasicBlock(expressions.get(0),
                    getFileName(firstExpr), codeExcerpt(expressions), false, lineCol);
            buildEdges(state, basicBlock);
            return basicBlock;
        }
        throw new NotImplementedException(null, "for empty expressions list");
    }

    private CFGBasicBlock newEntryBlock(MethodDeclaration node) {
        return CFGBasicBlock.buildEntryBlock(node, getFileName(node),
                codeExcerpt(node));
    }

    private CFGBasicBlock newExitBlock(MethodDeclaration node) {
        final LineAndColumn lineCol = getLineAndColumn(node
                .getStartPosition() + node.getLength());
        return CFGBasicBlock.buildExitBlock(node, getFileName(node),
                codeExcerpt(node), lineCol);
    }

    private LineAndColumn getLineAndColumn(ASTNode node) {
        return getLineAndColumn(node.getStartPosition());
    }

    private LineAndColumn getLineAndColumn(final int position) {
        // TODO Use CompilationUnit.getLineNumber() and CompilationUnit.getColumnNumber()
        // Return SourceLocation class with also startNodePosition to be used for graph node names
        // line number and column number are then used as comments for the node
        // file starts with line 1
        int lineNo = 1;
        int lastMatchPosition = 0;
        final Matcher matcher = NEWLINE.matcher(source);
        while (matcher.find()) {
            final MatchResult matchResult = matcher.toMatchResult();
            if (matchResult.end() >= position) {
                final String startOfLine = this.source.substring(
                        lastMatchPosition, position);
                final int nbChars = countCharacters(startOfLine, tabSize);
                // + 1 because line starts with column 1
                return new LineAndColumn(position, lineNo, nbChars + 1);
            }
            lastMatchPosition = matchResult.end();
            ++lineNo;
        }
        throw new IllegalStateException(null, "A line and column number should have been found");
    }

    private int countCharacters(String s, int tabSize) {
        int result = 0;
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) == '\t') {
                result += tabSize - (i % tabSize);
            } else {
                result++;
            }
        }
        return result;
    }

    private boolean isNotEmpty(final Collection<?> col) {
        return col != null && !col.isEmpty();
    }

    private boolean isNotEmpty(final Map<?, ?> col) {
        return col != null && !col.isEmpty();
    }

}
