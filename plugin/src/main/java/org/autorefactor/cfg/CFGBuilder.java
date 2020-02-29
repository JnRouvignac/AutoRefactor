/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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

import static org.eclipse.jdt.core.dom.ASTNode.ARRAY_ACCESS;
import static org.eclipse.jdt.core.dom.ASTNode.ARRAY_CREATION;
import static org.eclipse.jdt.core.dom.ASTNode.ARRAY_INITIALIZER;
import static org.eclipse.jdt.core.dom.ASTNode.ASSERT_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.BLOCK;
import static org.eclipse.jdt.core.dom.ASTNode.BOOLEAN_LITERAL;
import static org.eclipse.jdt.core.dom.ASTNode.BREAK_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.CAST_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.CHARACTER_LITERAL;
import static org.eclipse.jdt.core.dom.ASTNode.CLASS_INSTANCE_CREATION;
import static org.eclipse.jdt.core.dom.ASTNode.CONDITIONAL_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.CONSTRUCTOR_INVOCATION;
import static org.eclipse.jdt.core.dom.ASTNode.CONTINUE_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.DO_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.EMPTY_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.ENHANCED_FOR_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.EXPRESSION_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.FIELD_ACCESS;
import static org.eclipse.jdt.core.dom.ASTNode.FOR_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.IF_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.INFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.INSTANCEOF_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.LABELED_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.METHOD_INVOCATION;
import static org.eclipse.jdt.core.dom.ASTNode.NULL_LITERAL;
import static org.eclipse.jdt.core.dom.ASTNode.NUMBER_LITERAL;
import static org.eclipse.jdt.core.dom.ASTNode.PARENTHESIZED_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.POSTFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.PREFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.QUALIFIED_NAME;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.SIMPLE_NAME;
import static org.eclipse.jdt.core.dom.ASTNode.STRING_LITERAL;
import static org.eclipse.jdt.core.dom.ASTNode.SUPER_CONSTRUCTOR_INVOCATION;
import static org.eclipse.jdt.core.dom.ASTNode.SUPER_FIELD_ACCESS;
import static org.eclipse.jdt.core.dom.ASTNode.SUPER_METHOD_INVOCATION;
import static org.eclipse.jdt.core.dom.ASTNode.SWITCH_CASE;
import static org.eclipse.jdt.core.dom.ASTNode.SWITCH_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.SYNCHRONIZED_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.THIS_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.THROW_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.TRY_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.TYPE_DECLARATION;
import static org.eclipse.jdt.core.dom.ASTNode.TYPE_LITERAL;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.WHILE_STATEMENT;

import java.lang.reflect.Method;
import java.util.ArrayList;
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

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.JavaProjectOptions;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.UnhandledException;
import org.autorefactor.util.Utils;
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

/**
 * Builds a CFG.
 * <p>
 * Look at {@link #buildCFG(IfStatement, LivenessState, ThrowerBlocks)} for a
 * javadoc for all the buildCFG() methods.
 * <p>
 * TODO JNR detect dead code by looking for empty live blocks list when visiting
 * a node + looking at if / while / etc. conditions and see if they resolve to a
 * constant
 */
public class CFGBuilder {
    private static final class LivenessState {
        /**
         * The currently live CFGBasicBlock. It can be null which means that further
         * analysis will first have to create a live CFGBasicBlock.
         */
        private final CFGBasicBlock liveBasicBlock;
        /**
         * The edges that are live on entering or after finishing analyzing a statement.
         */
        private final List<CFGEdgeBuilder> liveEdges= new LinkedList<>();

        private LivenessState() {
            this.liveBasicBlock= null;
        }

        private LivenessState(final CFGBasicBlock liveBasicBlock) {
            this.liveBasicBlock= liveBasicBlock;
        }

        private LivenessState(final CFGBasicBlock liveBasicBlock, final CFGEdgeBuilder liveEdge) {
            this.liveBasicBlock= liveBasicBlock;
            this.liveEdges.add(liveEdge);
        }

        private static LivenessState of(final List<CFGEdgeBuilder> liveEdges) {
            LivenessState result= new LivenessState();
            result.addAll(liveEdges);
            return result;
        }

        private static LivenessState of(final CFGEdgeBuilder liveEdge) {
            LivenessState result= new LivenessState();
            result.add(liveEdge);
            return result;
        }

        private LivenessState copyLiveBasicBlock() {
            return new LivenessState(liveBasicBlock);
        }

        private LivenessState copyLiveEdges() {
            return of(liveEdges);
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

        private void add(final CFGEdgeBuilder edge) {
            liveEdges.add(edge);
        }

        private void addAll(final Collection<CFGEdgeBuilder> edges) {
            liveEdges.addAll(edges);
        }

        private void addAll(final LivenessState state) {
            liveEdges.addAll(state.liveEdges);
        }

        @Override
        public String toString() {
            return "LivenessState [liveBasicBlock=" + liveBasicBlock + ", liveEdges=" + liveEdges + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
    }

    private static final Pattern NEWLINE= Pattern.compile("\r\n|\r|\n"); //$NON-NLS-1$

    private final String source;
    private final int tabSize;
    /**
     * Edges to be built after visiting the statement used as the key.
     * <p>
     * After a statement is visited, code checks whether there are edges to build
     * and creates them.
     * </p>
     * <p>
     * This is only useful when labels are used with break or continue statements
     * which can send control flow back to any parent statement.
     * </p>
     */
    private final Map<Statement, Map<CFGEdgeBuilder, Boolean>> edgesToBuild= new HashMap<>();
    /** The exit block for the CFG being built. */
    private CFGBasicBlock exitBlock;

    /**
     * Cache of "manually" resolved type bindings.
     * <p>
     * Cannot be made static because for {@link ITypeBinding#equals(Object)} to
     * work, all type bindings must have been loaded from the same CompilationUnit.
     */
    private final Map<String, ITypeBinding> typeBindingsCache= new HashMap<>();

    /**
     * Builds an instance of this class.
     *
     * @param source  the java source code to work from
     * @param options the Java project options used to compile the project
     */
    public CFGBuilder(final String source, final JavaProjectOptions options) {
        this.source= source;
        this.tabSize= options.getTabSize();
    }

    /**
     * @return whether the current variable access can throw an exception.
     */
    @SuppressWarnings("unchecked")
    private boolean addVariableAccess(final CFGBasicBlock basicBlock, final Expression node, final int flags, final ThrowerBlocks throwers) {
        if (node == null) {
            return false;
        }

        switch (node.getNodeType()) {
        case ARRAY_ACCESS:
            ArrayAccess aa= (ArrayAccess) node;
            addVariableAccess(basicBlock, aa.getArray(), flags, throwers);
            addVariableAccess(basicBlock, aa.getIndex(), flags, throwers);
            throwers.addThrow(aa, newException(node, ArrayIndexOutOfBoundsException.class.getCanonicalName()));
            return true;

        case ARRAY_CREATION:
            ArrayCreation ac= (ArrayCreation) node;
            boolean acMightThrow1= addVariableAccess(basicBlock, ac.getInitializer(), flags, throwers);
            boolean acMightThrow2= addVariableAccesses(basicBlock, ac.dimensions(), flags, throwers);
            return acMightThrow1 || acMightThrow2;

        case ARRAY_INITIALIZER:
            ArrayInitializer ai= (ArrayInitializer) node;
            return addVariableAccesses(basicBlock, ai.expressions(), flags, throwers);

        case ASSIGNMENT:
            Assignment a= (Assignment) node;
            boolean aMightThrow1= addVariableAccess(basicBlock, a.getLeftHandSide(), VariableAccess.WRITE, throwers);
            boolean aMightThrow2= addVariableAccess(basicBlock, a.getRightHandSide(), VariableAccess.READ, throwers);
            return aMightThrow1 || aMightThrow2;

        case BOOLEAN_LITERAL:
        case CHARACTER_LITERAL:
        case NULL_LITERAL:
        case NUMBER_LITERAL:
        case STRING_LITERAL:
        case TYPE_LITERAL:
            // Nothing to do
            return false;

        case CAST_EXPRESSION:
            CastExpression cae= (CastExpression) node;
            return addVariableAccess(basicBlock, cae.getExpression(), flags, throwers);

        case CLASS_INSTANCE_CREATION:
            ClassInstanceCreation cic= (ClassInstanceCreation) node;
            addVariableAccess(basicBlock, cic.getExpression(), flags, throwers);
            addVariableAccesses(basicBlock, cic.arguments(), flags, throwers);
            IMethodBinding cicBinding= cic.resolveConstructorBinding();

            if (cicBinding != null) {
                ITypeBinding[] declaredThrows= cicBinding.getExceptionTypes();
                throwers.addThrow(cic, declaredThrows);
                return declaredThrows.length > 0;
            }

            return false;

        case CONDITIONAL_EXPRESSION:
            ConditionalExpression coe= (ConditionalExpression) node;
            boolean mightThrow1= addVariableAccess(basicBlock, coe.getExpression(), flags, throwers);
            boolean mightThrow2= addVariableAccess(basicBlock, coe.getThenExpression(), flags, throwers);
            boolean mightThrow3= addVariableAccess(basicBlock, coe.getElseExpression(), flags, throwers);
            return mightThrow1 || mightThrow2 || mightThrow3;

        case FIELD_ACCESS:
            FieldAccess fa= (FieldAccess) node;
            boolean mightThrow= addVariableAccess(basicBlock, fa.getExpression(), flags, throwers);
            basicBlock.addVariableAccess(new VariableAccess(fa, flags));

            if (is(flags, VariableAccess.READ)) {
                throwers.addThrow(fa, newException(node, NullPointerException.class.getCanonicalName()));
                mightThrow= true;
            }

            return mightThrow;

        case INFIX_EXPRESSION:
            InfixExpression ie= (InfixExpression) node;
            boolean ieMightThrow1= addVariableAccess(basicBlock, ie.getLeftOperand(), flags, throwers);
            boolean ieMightThrow2= addVariableAccess(basicBlock, ie.getRightOperand(), flags, throwers);
            return ieMightThrow1 || ieMightThrow2;

        case INSTANCEOF_EXPRESSION:
            InstanceofExpression ioe= (InstanceofExpression) node;
            return addVariableAccess(basicBlock, ioe.getLeftOperand(), flags, throwers);

        case METHOD_INVOCATION:
            MethodInvocation mi= (MethodInvocation) node;
            addVariableAccess(basicBlock, mi.getExpression(), flags, throwers);
            addVariableAccesses(basicBlock, mi.arguments(), flags, throwers);
            IMethodBinding methodBinding= mi.resolveMethodBinding();

            if (methodBinding != null) {
                ITypeBinding[] declaredThrows= methodBinding.getExceptionTypes();
                throwers.addThrow(mi, declaredThrows);
                return declaredThrows.length > 0;
            }

            return false;

        case SIMPLE_NAME:
            SimpleName sn= (SimpleName) node;
            basicBlock.addVariableAccess(new VariableAccess(sn, flags));

            if (is(flags, VariableAccess.READ)) {
                throwers.addThrow(sn, newException(node, NullPointerException.class.getCanonicalName()));
                return true;
            }

            return false;

        case QUALIFIED_NAME:
            QualifiedName qn= (QualifiedName) node;
            basicBlock.addVariableAccess(new VariableAccess(qn, flags));
            throwers.addThrow(qn, newException(node, NullPointerException.class.getCanonicalName()));
            return true;

        case PARENTHESIZED_EXPRESSION:
            ParenthesizedExpression pe= (ParenthesizedExpression) node;
            return addVariableAccess(basicBlock, pe.getExpression(), flags, throwers);

        case POSTFIX_EXPRESSION:
            PostfixExpression poe= (PostfixExpression) node;
            return addVariableAccess(basicBlock, poe.getOperand(), flags, throwers);

        case PREFIX_EXPRESSION:
            PrefixExpression pre= (PrefixExpression) node;
            return addVariableAccess(basicBlock, pre.getOperand(), flags, throwers);

        case SUPER_FIELD_ACCESS:
            SuperFieldAccess sfa= (SuperFieldAccess) node;
            boolean sfaMightThrow1= addVariableAccess(basicBlock, sfa.getQualifier(), flags, throwers);
            boolean sfaMightThrow2= addVariableAccess(basicBlock, sfa.getName(), flags, throwers);
            return sfaMightThrow1 || sfaMightThrow2;

        case SUPER_METHOD_INVOCATION:
            SuperMethodInvocation smi= (SuperMethodInvocation) node;
            addVariableAccess(basicBlock, smi.getQualifier(), flags, throwers);
            addVariableAccess(basicBlock, smi.getName(), flags, throwers);
            IMethodBinding sMethodBinding= smi.resolveMethodBinding();

            if (sMethodBinding != null) {
                ITypeBinding[] declaredThrows= sMethodBinding.getExceptionTypes();
                throwers.addThrow(smi, declaredThrows);
                return declaredThrows.length > 0;
            }

            return false;

        case THIS_EXPRESSION:
            ThisExpression te= (ThisExpression) node;
            // TODO JNR remember use of "this" here
            return addVariableAccess(basicBlock, te.getQualifier(), flags, throwers);

        case VARIABLE_DECLARATION_EXPRESSION:
            return addDeclarations(basicBlock, (VariableDeclarationExpression) node, throwers);

        default:
            throw new NotImplementedException(node);
        }
    }

    private boolean is(final int flags, final int flag) {
        return (flags & flag) == flag;
    }

    private ITypeBinding newException(final Expression node, final String fullyQualifiedName) {
        ITypeBinding typeBinding= typeBindingsCache.get(fullyQualifiedName);
        if (typeBinding == null) {
            typeBinding= new TypeNameDecider(node).resolveTypeBinding(fullyQualifiedName);
            typeBindingsCache.put(typeBinding.getQualifiedName(), typeBinding);
        }

        return typeBinding;
    }

    /**
     * @return whether the current variable accesses can throw an exception.
     */
    private boolean addVariableAccesses(final CFGBasicBlock basicBlock, final List<Expression> expressions, final int flags,
            final ThrowerBlocks throwers) {
        boolean mightThrow= false;
        for (Expression expression : expressions) {
            if (addVariableAccess(basicBlock, expression, flags, throwers)) {
                mightThrow= true;
            }
        }

        return mightThrow;
    }

    private boolean addDeclarations(final CFGBasicBlock basicBlock, final List<VariableDeclarationFragment> fragments, final Type type,
            final ThrowerBlocks throwers) {
        boolean mightThrow= false;
        for (VariableDeclarationFragment vdf : fragments) {
            if (addDeclaration(basicBlock, vdf, type, throwers)) {
                mightThrow= true;
            }
        }

        return mightThrow;
    }

    private boolean addDeclaration(final CFGBasicBlock basicBlock, final VariableDeclarationFragment vdf, final Type type,
            final ThrowerBlocks throwers) {
        int accessType= vdf.getInitializer() == null ? VariableAccess.DECL_UNINIT : VariableAccess.DECL_INIT | VariableAccess.WRITE;
        basicBlock.addVariableAccess(new VariableAccess(vdf, vdf.getName(), type, accessType));
        return addVariableAccess(basicBlock, vdf.getInitializer(), VariableAccess.READ, throwers);
    }

    private boolean addDeclarations(final CFGBasicBlock basicBlock, final VariableDeclarationExpression vde,
            final ThrowerBlocks throwers) {
        return addDeclarations(basicBlock, ASTNodes.fragments(vde), vde.getType(), throwers);
    }

    private void addDeclarations(final CFGBasicBlock basicBlock, final List<SingleVariableDeclaration> varDecls) {
        for (SingleVariableDeclaration varDecl : varDecls) {
            addDeclaration(basicBlock, varDecl);
        }
    }

    private void addDeclaration(final CFGBasicBlock basicBlock, final SingleVariableDeclaration varDecl) {
        addDeclaration(basicBlock, varDecl, VariableAccess.DECL_INIT);
    }

    private void addDeclaration(final CFGBasicBlock basicBlock, final SingleVariableDeclaration varDecl, final int flags) {
        basicBlock.addVariableAccess(new VariableAccess(varDecl, varDecl.getName(), varDecl.getType(), flags));
    }

    private LivenessState buildCFG(final Statement node, final LivenessState state, final ThrowerBlocks throwers) {
        if (node == null) {
            return state.nextStmtWillCreateNewBlock();
        }
        try {
            Method m= getClass().getMethod("buildCFG", node.getClass(), LivenessState.class, ThrowerBlocks.class); //$NON-NLS-1$
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
    public void buildCFG(final QualifiedName node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final PrimitiveType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final QualifiedType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final PrefixExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final PostfixExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final ParenthesizedExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final SingleVariableDeclaration node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final SimpleType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final SimpleName node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final ReturnStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        if (node.getExpression() != null) {
            addVariableAccess(basicBlock, node.getExpression(), VariableAccess.READ, throwers);
        }
        CFGEdgeBuilder.buildEdge(basicBlock, this.exitBlock);
        return state.nextStmtsAreDeadCode();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final Modifier node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final MethodInvocation node) {
        // TODO JNR add variable access to "this"
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @return the entry block to the CFG of this method declaration
     */
    public CFGBasicBlock buildCFG(final MethodDeclaration node) {
        CFGBasicBlock entryBlock= newEntryBlock(node);
        this.exitBlock= newExitBlock(node);

        addDeclarations(entryBlock, ASTNodes.parameters(node));

        try {
            ThrowerBlocks throwers= new ThrowerBlocks();
            CFGEdgeBuilder liveEdge= new CFGEdgeBuilder(entryBlock);
            LivenessState liveAfterBody= buildCFG(node.getBody(), LivenessState.of(liveEdge), throwers);
            if (!liveAfterBody.liveEdges.isEmpty()) {
                if (node.getReturnType2() != null && node.getReturnType2().resolveBinding() != null /* added for unit Tests */ && !"void".equals(node.getReturnType2().resolveBinding().getName())) { //$NON-NLS-1$
                    throw new IllegalStateException(node, "Did not expect to find any edges to build " //$NON-NLS-1$
                            + "for a constructor or a non void method return type."); //$NON-NLS-1$
                }
                buildEdges(liveAfterBody, exitBlock);
            }
            if (!this.edgesToBuild.isEmpty()) {
                throw new IllegalStateException(node,
                        "At this point, there should not be any edges left to build. Left edges: " + this.edgesToBuild); //$NON-NLS-1$
            }
            List<CFGBasicBlock> throwingBlocks= throwers.selectBlocksThrowing(null);
            if (!throwingBlocks.isEmpty()) {
                for (CFGBasicBlock throwingBlock : throwingBlocks) {
                    // TODO JNR
                }
            }
            List<CFGEdgeBuilder> throwingEdges= throwers.selectEdgesThrowing(null);
            if (!throwingEdges.isEmpty()) {
                for (CFGEdgeBuilder throwingEdge : throwingEdges) {
                    // TODO JNR
                }
            }

            return entryBlock;
        } finally {
            this.exitBlock= null;
        }
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final MethodRefParameter node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final MethodRef node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final MemberValuePair node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final ParameterizedType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final NumberLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final NullLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final UnionType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final TypeParameter node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final TypeLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final TypeDeclarationStatement node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @return the list of basic blocks representing CFGs for each method in this
     *         type declaration
     */
    public List<CFGBasicBlock> buildCFG(final TypeDeclaration node) {
        if (!node.isInterface()) {
            List<CFGBasicBlock> results= new LinkedList<>();
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
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final TryStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        ThrowerBlocks localThrowers= new ThrowerBlocks();

        LivenessState liveAfterTry= buildCFG(node.getBody(), state, localThrowers);
        LivenessState liveAfterCatchClauses= new LivenessState();

        Set<ITypeBinding> caughtExceptions= new HashSet<>();
        for (CatchClause catchClause : ASTNodes.catchClauses(node)) {
            LivenessState catchState= new LivenessState();
            CFGBasicBlock catchBasicBlock= getCFGBasicBlock(catchClause, catchState);
            SingleVariableDeclaration exceptionDecl= catchClause.getException();
            addDeclaration(catchBasicBlock, exceptionDecl, VariableAccess.DECL_INIT);

            ITypeBinding caughtException= exceptionDecl.getType().resolveBinding();
            caughtExceptions.add(caughtException);

            List<CFGBasicBlock> throwingBlocksInTry= localThrowers.selectBlocksThrowing(caughtException);
            if (throwingBlocksInTry.isEmpty()) {
                // TODO JNR dead code found!!
            }
            List<CFGEdgeBuilder> liveBeforeCatchClause= new ArrayList<>(throwingBlocksInTry.size());
            for (CFGBasicBlock throwingBlockInTry : throwingBlocksInTry) {
                // TODO JNR if a Statement throws an exception, it must break the current
                // basicBlock
                // TODO JNR how to specify this edge is due to an exception?
                liveBeforeCatchClause.add(new CFGEdgeBuilder(throwingBlockInTry));
            }

            LivenessState liveAfterCatchClause= buildCFG(catchClause.getBody(), catchState, new ThrowerBlocks());
            liveAfterCatchClauses.addAll(liveAfterCatchClause);
        }

        // TODO JNR move uncaught exceptions from localThrowers to throwers
        Map<CFGBasicBlock, Set<ITypeBinding>> throwUncaughtExceptions= localThrowers
                .selectBlocksThrowingOtherThan(caughtExceptions);
        for (Entry<CFGBasicBlock, Set<ITypeBinding>> throwing : throwUncaughtExceptions.entrySet()) {
            CFGEdgeBuilder uncaughtExceptionEdge= new CFGEdgeBuilder(throwing.getKey(), true);
            liveAfterCatchClauses.add(uncaughtExceptionEdge);
            throwers.addThrow(uncaughtExceptionEdge, throwing.getValue());
        }

        if (node.getFinally() != null) {
            LivenessState liveBeforeFinally= new LivenessState();
            liveBeforeFinally.addAll(liveAfterTry);
            liveBeforeFinally.addAll(liveAfterCatchClauses);
            return buildCFG(node.getFinally(), liveBeforeFinally, throwers);
        }

        return liveAfterCatchClauses.copyLiveBasicBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final WildcardType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final WhileStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock conditionBlock= getCFGBasicBlock(node.getExpression(), state.nextStmtWillCreateNewBlock());
        addVariableAccess(conditionBlock, node.getExpression(), VariableAccess.READ, throwers);

        CFGEdgeBuilder liveEdge= new CFGEdgeBuilder(node.getExpression(), true, conditionBlock);
        LivenessState liveAfterStatement= buildCFG(node.getBody(), LivenessState.of(liveEdge), throwers);
        liveAfterStatement.add(new CFGEdgeBuilder(node.getExpression(), false, conditionBlock));
        buildEdgesAfterBranchableStatement(node, liveAfterStatement, conditionBlock);
        return liveAfterStatement.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final VariableDeclarationFragment node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final VariableDeclarationStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        addDeclarations(basicBlock, ASTNodes.fragments(node), node.getType(), throwers);
        return getInBlockStmtResult(state, basicBlock);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final VariableDeclarationExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final SwitchStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        LivenessState liveBeforeBody= new LivenessState(basicBlock, new CFGEdgeBuilder(basicBlock));
        LivenessState liveAfterBody= buildCFG(ASTNodes.statements(node), liveBeforeBody, throwers);
        liveAfterBody.add(new CFGEdgeBuilder(basicBlock));

        buildEdgesAfterBranchableStatement(node, liveAfterBody, basicBlock);
        return liveAfterBody.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node                      the node for which to build a CFG.
     * @param switchConditionBasicBlock the basic block for the switch condition
     * @param state                     the blocks liveness state before current
     *                                  node
     * @param throwers                  the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final SwitchCase node, final CFGBasicBlock switchConditionBasicBlock, final LivenessState state,
            final ThrowerBlocks throwers) {
        // The current live blocks will be empty if there was a break,
        // or populated in case of fall-through.
        addVariableAccess(switchConditionBasicBlock, node.getExpression(), VariableAccess.READ, throwers);
        // Add an edge going from the condition of the switch
        // (state.liveBasicBlock is the condition of the switch)
        state.add(new CFGEdgeBuilder(node.getExpression(), true, switchConditionBasicBlock));
        return state.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final SuperMethodInvocation node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final SuperFieldAccess node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final SuperConstructorInvocation node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        addVariableAccesses(basicBlock, ASTNodes.arguments(node), VariableAccess.READ, throwers);
        return getInBlockStmtResult(state, basicBlock);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final StringLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final ThrowStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        Expression throwingExpression= node.getExpression();
        addVariableAccess(state.liveBasicBlock, throwingExpression, VariableAccess.READ, throwers);
        throwers.addThrow(basicBlock, throwingExpression.resolveTypeBinding());
        return state.nextStmtsAreDeadCode();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final ThisExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final TextElement node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final TagElement node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final SynchronizedStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());
        addVariableAccess(basicBlock, node.getExpression(), VariableAccess.READ, throwers);
        CFGEdgeBuilder liveEdge= new CFGEdgeBuilder(basicBlock);
        LivenessState result= buildCFG(node.getBody(), LivenessState.of(liveEdge), throwers);
        return result.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final CatchClause node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final CastExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final BreakStatement node, final LivenessState state) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        Statement targetStatement;
        if (node.getLabel() != null) {
            targetStatement= findLabeledParentStatement(node);
        } else {
            targetStatement= findBreakableParentStatement(node);
        }
        addEdgeToBuild(targetStatement, new CFGEdgeBuilder(basicBlock), true);
        return state.copyLiveBasicBlock();
    }

    private Statement findLabeledParentStatement(final ASTNode node) {
        ASTNode n= node;
        while (n != null && n.getNodeType() != LABELED_STATEMENT) {
            n= n.getParent();
        }
        if (n != null) {
            return ((LabeledStatement) n).getBody();
        }

        return null;
    }

    private Statement findBreakableParentStatement(final ASTNode node) {
        ASTNode n= node;
        while (n != null && !ASTNodes.isBreakable(n)) {
            n= n.getParent();
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
    public void buildCFG(final BooleanLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final ConstructorInvocation node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        addVariableAccesses(basicBlock, ASTNodes.arguments(node), VariableAccess.READ, throwers);
        return getInBlockStmtResult(state, basicBlock);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final ConditionalExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     * @return the list of basic blocks representing CFGs for each method in this
     *         compilation unit
     */
    @SuppressWarnings("unchecked")
    public List<CFGBasicBlock> buildCFG(final CompilationUnit node) {
        List<CFGBasicBlock> results= new LinkedList<>();
        for (AbstractTypeDeclaration decl : (List<AbstractTypeDeclaration>) node.types()) {
            if (decl.getNodeType() != TYPE_DECLARATION) {
                throw new NotImplementedException(node);
            }
            results.addAll(buildCFG((TypeDeclaration) decl));
        }

        return results;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final ClassInstanceCreation node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final CharacterLiteral node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final ArrayCreation node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final ArrayAccess node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final AnonymousClassDeclaration node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final Block node, final LivenessState state, final ThrowerBlocks throwers) {
        LivenessState liveState= state;
        try {
            liveState= buildCFG(ASTNodes.statements(node), state, throwers);
        } finally {
            moveAllEdgesToBuild(node, liveState);
        }

        return liveState.nextStmtWillCreateNewBlock();
    }

    private LivenessState buildCFG(final List<Statement> statements, final LivenessState startState, final ThrowerBlocks throwers) {
        LivenessState liveState= startState;
        for (Statement statement : statements) {
            switch (statement.getNodeType()) {
            case ASSERT_STATEMENT:
                liveState= buildCFG((AssertStatement) statement, liveState, throwers);
                break;

            case BLOCK:
                liveState= buildCFG((Block) statement, liveState, throwers);
                break;

            case BREAK_STATEMENT:
                liveState= buildCFG((BreakStatement) statement, liveState);
                break;

            case CONSTRUCTOR_INVOCATION:
            case SUPER_CONSTRUCTOR_INVOCATION:
                liveState= buildCFG(statement, liveState, throwers);
                break;

            case CONTINUE_STATEMENT:
                liveState= buildCFG((ContinueStatement) statement, liveState);
                break;

            case DO_STATEMENT:
                liveState= buildCFG((DoStatement) statement, liveState, throwers);
                break;

            case EMPTY_STATEMENT:
                liveState= buildCFG((EmptyStatement) statement, liveState);
                break;

            case ENHANCED_FOR_STATEMENT:
                liveState= buildCFG((EnhancedForStatement) statement, liveState, throwers);
                break;

            case EXPRESSION_STATEMENT:
                liveState= buildCFG((ExpressionStatement) statement, liveState, throwers);
                break;

            case FOR_STATEMENT:
                liveState= buildCFG((ForStatement) statement, liveState, throwers);
                break;

            case IF_STATEMENT:
                liveState= buildCFG((IfStatement) statement, liveState, throwers);
                break;

            case LABELED_STATEMENT:
                liveState= buildCFG((LabeledStatement) statement, liveState, throwers);
                break;

            case RETURN_STATEMENT:
                liveState= buildCFG((ReturnStatement) statement, liveState, throwers);
                break;

            case SWITCH_CASE:
                // Here, use startState.liveBasicBlock to build an edge
                // from the switch condition to the case statement
                liveState= buildCFG((SwitchCase) statement, startState.liveBasicBlock, liveState, throwers);
                break;

            case SWITCH_STATEMENT:
                liveState= buildCFG((SwitchStatement) statement, liveState, throwers);
                break;

            case SYNCHRONIZED_STATEMENT:
                liveState= buildCFG((SynchronizedStatement) statement, liveState, throwers);
                break;

            case THROW_STATEMENT:
                liveState= buildCFG((ThrowStatement) statement, liveState, throwers);
                break;

            case TRY_STATEMENT:
                liveState= buildCFG((TryStatement) statement, liveState, throwers);
                // break;case TYPE_DECLARATION_STATEMENT:
                // buildCFG((TypeDeclarationStatement) statement, liveState, throwers);
                break;

            case VARIABLE_DECLARATION_STATEMENT:
                liveState= buildCFG((VariableDeclarationStatement) statement, liveState, throwers);
                break;

            case WHILE_STATEMENT:
                liveState= buildCFG((WhileStatement) statement, liveState, throwers);
                break;

            default:
                throw new NotImplementedException(statement);
            }
        }

        return liveState;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final Assignment node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final AssertStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        addVariableAccess(basicBlock, node.getExpression(), VariableAccess.READ, throwers);
        addVariableAccess(basicBlock, node.getMessage(), VariableAccess.READ, throwers);
        return getInBlockStmtResult(state, basicBlock);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final ArrayType node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final ArrayInitializer node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final Initializer node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final InstanceofExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final InfixExpression node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the passed in statement.
     *
     * @param node     the statement for which to build a CFG
     * @param state    the liveness state before the current statement. It contains:
     *                 the live edges before the current statement, the live basic
     *                 block to which the current statement might be added. If null,
     *                 then the new basic block must be created for the current
     *                 statement.
     * @param throwers the thrower blocks information
     * @return the new live state after the current statement
     */
    public LivenessState buildCFG(final IfStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock exprBlock= getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock(), true);
        try {
            addVariableAccess(exprBlock, node.getExpression(), VariableAccess.READ, throwers);

            LivenessState result= new LivenessState();
            CFGEdgeBuilder thenEdge= new CFGEdgeBuilder(node.getExpression(), true, exprBlock);
            result.addAll(buildCFG(node.getThenStatement(), LivenessState.of(thenEdge), throwers));

            Statement elseStatement= node.getElseStatement();
            CFGEdgeBuilder elseEdge= new CFGEdgeBuilder(node.getExpression(), false, exprBlock);
            if (elseStatement != null) {
                result.addAll(buildCFG(elseStatement, LivenessState.of(elseEdge), throwers));
            } else {
                result.add(elseEdge);
            }

            return result.nextStmtWillCreateNewBlock();
        } finally {
            moveAllEdgesToBuild(node, state);
        }
    }

    private void addEdgeToBuild(final Statement node, final CFGEdgeBuilder builder, final boolean isBreakStatement) {
        if (builder != null) {
            Map<CFGEdgeBuilder, Boolean> builders= this.edgesToBuild.get(node);
            if (builders == null) {
                builders= new HashMap<>();
                this.edgesToBuild.put(node, builders);
            }
            builders.put(builder, isBreakStatement);
        }
    }

    private void moveAllEdgesToBuild(final Statement node, final LivenessState state) {
        Map<CFGEdgeBuilder, Boolean> toBuild= this.edgesToBuild.remove(node);
        if (toBuild != null) {
            state.addAll(toBuild.keySet());
        }
    }

    private void buildEdges(final LivenessState toBuild, final CFGBasicBlock targetBlock) {
        if (!Utils.isEmpty(toBuild.liveEdges)) {
            for (CFGEdgeBuilder builder : toBuild.liveEdges) {
                builder.withTarget(targetBlock).build();
            }
            toBuild.liveEdges.clear();
        }
    }

    private void buildEdgesAfterBranchableStatement(final Statement node, final LivenessState liveAfterBranchableStatement,
            final CFGBasicBlock whereToBranchBlock) {
        Map<CFGEdgeBuilder, Boolean> toBuild= this.edgesToBuild.remove(node);
        if (isNotEmpty(toBuild)) {
            Set<Entry<CFGEdgeBuilder, Boolean>> set= toBuild.entrySet();
            for (Iterator<Entry<CFGEdgeBuilder, Boolean>> iter= set.iterator(); iter.hasNext();) {
                Entry<CFGEdgeBuilder, Boolean> entry= iter.next();
                CFGEdgeBuilder builder= entry.getKey();
                boolean isBreakStatement= entry.getValue();
                if (isBreakStatement) {
                    liveAfterBranchableStatement.add(builder);
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
    public void buildCFG(final MemberRef node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final LabeledStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        // Does not count as an executable node, so do not get a basic block for it
        return buildCFG(node.getBody(), state, throwers);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final EnhancedForStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());

        addDeclaration(basicBlock, node.getParameter(), VariableAccess.DECL_INIT | VariableAccess.WRITE);

        LivenessState newLiveState= LivenessState.of(new CFGEdgeBuilder(basicBlock));
        LivenessState liveAfterBody= buildCFG(node.getBody(), newLiveState, throwers);
        buildEdges(liveAfterBody, basicBlock);

        LivenessState liveAfterStatement= LivenessState.of(new CFGEdgeBuilder(basicBlock));
        buildEdgesAfterBranchableStatement(node, liveAfterStatement, basicBlock);
        return liveAfterStatement.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final EmptyStatement node, final LivenessState state) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        return getInBlockStmtResult(state, basicBlock);
    }

    private LivenessState getInBlockStmtResult(final LivenessState state, final CFGBasicBlock basicBlock) {
        if (state.liveBasicBlock == null) {
            // New block was created for current node
            return new LivenessState(basicBlock, new CFGEdgeBuilder(basicBlock));
        }

        return state;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final DoStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());
        LivenessState newLiveState= new LivenessState(basicBlock, new CFGEdgeBuilder(basicBlock));
        LivenessState liveAfterLoop= buildCFG(node.getBody(), newLiveState, throwers);
        CFGBasicBlock conditionBlock= getCFGBasicBlock(node.getExpression(),
                liveAfterLoop.nextStmtWillCreateNewBlock());
        addVariableAccess(conditionBlock, node.getExpression(), VariableAccess.READ, throwers);

        CFGEdgeBuilder.buildEdge(node.getExpression(), true, conditionBlock, basicBlock);

        LivenessState liveAfterStatement= LivenessState.of(new CFGEdgeBuilder(node.getExpression(), false, conditionBlock));
        buildEdgesAfterBranchableStatement(node, liveAfterStatement, basicBlock);
        return liveAfterStatement;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final ContinueStatement node, final LivenessState state) {
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        Statement targetStatement;
        if (node.getLabel() != null) {
            targetStatement= findLabeledParentStatement(node);
        } else {
            targetStatement= findContinuableParentStatement(node);
        }
        addEdgeToBuild(targetStatement, new CFGEdgeBuilder(basicBlock), false);
        return state.copyLiveBasicBlock();
    }

    private Statement findContinuableParentStatement(final ASTNode node) {
        ASTNode n= node;
        while (n != null && !ASTNodes.isLoop(n)) {
            n= n.getParent();
        }
        if (n != null) {
            return (Statement) n;
        }

        return null;
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final ForStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        CFGBasicBlock initBlock= getCFGBasicBlock(ASTNodes.initializers(node), state);
        LivenessState initLiveBlock= LivenessState.of(new CFGEdgeBuilder(initBlock));
        CFGBasicBlock exprBlock= getCFGBasicBlock(node.getExpression(), initLiveBlock, true);
        CFGBasicBlock updatersBlock= getCFGBasicBlock(ASTNodes.updaters(node), new LivenessState());
        CFGEdgeBuilder.buildEdge(updatersBlock, exprBlock);

        for (Expression expression : ASTNodes.initializers(node)) {
            if (expression instanceof VariableDeclarationExpression) {
                addDeclarations(initBlock, (VariableDeclarationExpression) expression, throwers);
            }
        }
        addVariableAccess(exprBlock, node.getExpression(), VariableAccess.READ, throwers);
        addVariableAccesses(updatersBlock, ASTNodes.updaters(node), VariableAccess.WRITE, throwers);

        CFGEdgeBuilder liveBlock= new CFGEdgeBuilder(node.getExpression(), true, exprBlock);
        LivenessState liveAfterBody= buildCFG(node.getBody(), LivenessState.of(liveBlock), throwers);
        buildEdges(liveAfterBody, updatersBlock);

        LivenessState liveAfterStatement= LivenessState.of(new CFGEdgeBuilder(node.getExpression(), false, exprBlock));
        buildEdgesAfterBranchableStatement(node, liveAfterStatement, updatersBlock);
        return liveAfterStatement.nextStmtWillCreateNewBlock();
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final FieldDeclaration node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node the node for which to build a CFG.
     */
    public void buildCFG(final FieldAccess node) {
        throw new NotImplementedException(node);
    }

    /**
     * Builds a CFG for the provided node.
     *
     * @param node     the node for which to build a CFG.
     * @param state    the blocks liveness state before current node
     * @param throwers the thrower blocks information
     * @return the blocks liveness state after current node
     */
    public LivenessState buildCFG(final ExpressionStatement node, final LivenessState state, final ThrowerBlocks throwers) {
        boolean isNewBlock= state.requireNewBlock();
        CFGBasicBlock basicBlock= getCFGBasicBlock(node, state);
        boolean mightThrow= addVariableAccess(basicBlock, node.getExpression(), VariableAccess.READ, throwers);
        if (!isNewBlock && mightThrow) {
            CFGBasicBlock currentBlock= getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());
            return new LivenessState(currentBlock, new CFGEdgeBuilder(currentBlock));
            // TODO JNR create a new CFGBasicBlock from here to catch / finally / exit
        }

        return getInBlockStmtResult(state, basicBlock);
    }

    private CFGBasicBlock getCFGBasicBlock(final ASTNode node, final LivenessState state) {
        return getCFGBasicBlock(node, state, false);
    }

    /**
     * Will create and return a new CFGBasicBlock for the passed in node, if the
     * liveBasicBlock is null, otherwise it will return the liveBasicBlock.
     *
     * @param node
     * @param state      the liveness state the current statement will be added to.
     *                   A null liveBasicBlock forces the creation of a new
     *                   CFGBasicBlock. liveEdges are the edges that are live before
     *                   getting the CFGBasicBlock
     * @param isDecision used for building the associated CFGEdge
     * @return
     */
    private CFGBasicBlock getCFGBasicBlock(final ASTNode node, final LivenessState state, final boolean isDecision) {
        Map<CFGEdgeBuilder, Boolean> toBuild= this.edgesToBuild.remove(node);
        if (isNotEmpty(toBuild)) {
            throw new IllegalStateException(node, "No edges to build should exist for node \"" + node //$NON-NLS-1$
                    + "\" before a CFGBasicBlock is created for it. Found the following edges to build " + toBuild); //$NON-NLS-1$
        }
        if (!state.requireNewBlock()) {
            // TODO JNR add nodes to the basicBlock they belong to
            // and adapt the CFGDotPrinter to display "..." after the first node
            // basicBlock.addNode(node);
            return state.liveBasicBlock;
        }
        LineAndColumn lineCol= getLineAndColumn(node);
        CFGBasicBlock basicBlock= new CFGBasicBlock(node, ASTNodes.getFileName(node), ASTPrintHelper.codeExcerpt(node), isDecision,
                lineCol);
        buildEdges(state, basicBlock);
        return basicBlock;
    }

    private CFGBasicBlock getCFGBasicBlock(final List<Expression> expressions, final LivenessState state) {
        if (!Utils.isEmpty(expressions)) {
            Expression firstExpression= expressions.get(0);
            LineAndColumn lineCol= getLineAndColumn(firstExpression.getStartPosition());
            CFGBasicBlock basicBlock= new CFGBasicBlock(expressions.get(0), ASTNodes.getFileName(firstExpression),
                    ASTPrintHelper.codeExcerpt(expressions), false, lineCol);
            buildEdges(state, basicBlock);
            return basicBlock;
        }
        throw new NotImplementedException(null, "for empty expressions list"); //$NON-NLS-1$
    }

    private CFGBasicBlock newEntryBlock(final MethodDeclaration node) {
        return CFGBasicBlock.buildEntryBlock(node, ASTNodes.getFileName(node), ASTPrintHelper.codeExcerpt(node));
    }

    private CFGBasicBlock newExitBlock(final MethodDeclaration node) {
        LineAndColumn lineCol= getLineAndColumn(node.getStartPosition() + node.getLength());
        return CFGBasicBlock.buildExitBlock(node, ASTNodes.getFileName(node), ASTPrintHelper.codeExcerpt(node), lineCol);
    }

    private LineAndColumn getLineAndColumn(final ASTNode node) {
        return getLineAndColumn(node.getStartPosition());
    }

    private LineAndColumn getLineAndColumn(final int position) {
        // TODO Use CompilationUnit.getLineNumber() and
        // CompilationUnit.getColumnNumber()
        // Return SourceLocation class with also startNodePosition to be used for graph
        // node names
        // line number and column number are then used as comments for the node
        // file starts with line 1
        int lineNo= 1;
        int lastMatchPosition= 0;
        Matcher matcher= NEWLINE.matcher(source);
        while (matcher.find()) {
            MatchResult matchResult= matcher.toMatchResult();
            if (matchResult.end() >= position) {
                String startOfLine= this.source.substring(lastMatchPosition, position);
                int nbChars= countCharacters(startOfLine, tabSize);
                // + 1 because line starts with column 1
                return new LineAndColumn(position, lineNo, nbChars + 1);
            }
            lastMatchPosition= matchResult.end();
            ++lineNo;
        }
        throw new IllegalStateException(null, "A line and column number should have been found"); //$NON-NLS-1$
    }

    private int countCharacters(final String s, final int tabSize) {
        int result= 0;
        for (int i= 0; i < s.length(); i++) {
            if (s.charAt(i) == '\t') {
                result+= tabSize - i % tabSize;
            } else {
                result++;
            }
        }

        return result;
    }

    private boolean isNotEmpty(final Map<?, ?> col) {
        return col != null && !col.isEmpty();
    }
}
