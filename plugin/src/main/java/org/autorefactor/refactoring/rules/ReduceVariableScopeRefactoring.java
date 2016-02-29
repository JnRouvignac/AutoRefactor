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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.IllegalArgumentException;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression.Operator;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.VariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.*;

/**
 * TODO JNR can we also transform singular fields into local variables?
 *
 * @see {@link #getDescription()}
 */
@SuppressWarnings("javadoc")
public class ReduceVariableScopeRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Reduces the scope of local variables.";
    }

    @Override
    public String getName() {
        return "Reduce scope of variable";
    }

    private static final int DECL  = 1 << 0;
    private static final int READ  = 1 << 1;
    private static final int WRITE = 1 << 2;

    private static final class VariableName {

        private final Name name;

        public VariableName(Name name) {
            this.name = name;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (!(obj instanceof VariableName)) {
                return false;
            }
            final VariableName other = (VariableName) obj;
            return isEqual(this.name, other.name);
        }

        @Override
        public int hashCode() {
            return hashCode0(name);
        }

        private int hashCode0(Name name) {
            switch (name.getNodeType()) {
            case SIMPLE_NAME:
                return ((SimpleName) name).getIdentifier().hashCode();

            case QUALIFIED_NAME:
                QualifiedName qn = (QualifiedName) name;
                return hashCode0(qn.getQualifier()) + qn.getName().getIdentifier().hashCode();

            default:
                throw new NotImplementedException(name, name);
            }
        }

        @Override
        public String toString() {
            return this.name.toString();
        }
    }

    private static final class VariableAccess {

        private final Name variableName;
        private final int accessType;
        private final ASTNode scope;

        public VariableAccess(Name variableName, int accessType, ASTNode scope) {
            if (accessType == 0) {
                throw new IllegalArgumentException(null, "accessType must not be null");
            }
            this.variableName = variableName;
            this.accessType = accessType;
            this.scope = scope;
        }

        public Name getVariableName() {
            return variableName;
        }

        public int getAccessType() {
            return accessType;
        }

        private boolean is(int accessType) {
            return (this.accessType & accessType) != 0;
        }

        public ASTNode getScope() {
            return scope;
        }

        private Statement getStatement() {
            return getAncestor(variableName, Statement.class);
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            sb.append(variableName).append(" / ");
            appendAccesses(sb);
            sb.append(" / ").append(getStatement());
            sb.append(scope);
            return sb.toString();
        }

        private void appendAccesses(final StringBuilder sb) {
            List<String> accesses = new ArrayList<String>();
            if (is(DECL)) {
                accesses.add("DECL");
            }
            if (is(READ)) {
                accesses.add("READ");
            }
            if (is(WRITE)) {
                accesses.add("WRITE");
            }
            Iterator<String> it = accesses.iterator();
            sb.append(it.next());
            while (it.hasNext()) {
                sb.append("+").append(it.next());
            }
        }
    }

    private static final Pair<Integer, ASTNode> NULL_PAIR = Pair.of(0, null);
    private final Map<VariableName, List<VariableAccess>> allVariableAccesses =
            new HashMap<VariableName, List<VariableAccess>>();

    @Override
    public void endVisit(CompilationUnit node) {
        allVariableAccesses.clear();
    }

    @Override
    public void postVisit(ASTNode node) {
        if (node.getNodeType() != BLOCK) {
            return;
        }

        List<VariableAccess> variableAccesses = getVariablesWithScope(node);
        Map<IVariableBinding, List<VariableAccess>> iVarAccesses = new HashMap<IVariableBinding, List<VariableAccess>>();
        if (!variableAccesses.isEmpty()) {
            for (VariableAccess access : variableAccesses) {
                Name varName = access.variableName;
                if (varName instanceof SimpleName) {
                    SimpleName name = (SimpleName) varName;
                    // TODO JNR remove this loop?
                    IBinding b = name.resolveBinding();
                    if (b != null && b.getKind() == IBinding.VARIABLE) {
                        IVariableBinding varB = (IVariableBinding) b;
                        List<VariableAccess> list = iVarAccesses.get(varB);
                        if (list == null) {
                            list = new ArrayList<VariableAccess>();
                            iVarAccesses.put(varB, list);
                        }
                        list.add(access);
                    }
                }
            }
        }

        for (List<VariableAccess> variableAccesses2 : iVarAccesses.values()) {
            for (ListIterator<VariableAccess> it = variableAccesses2.listIterator(); it.hasNext();) {
                VariableAccess access1 = it.next();
                if (access1.is(WRITE)) {
                    Statement stmt1 = access1.getStatement();
                    if (!it.hasNext()) {
                        if (access1.is(READ)) {
                            // this a pre/post inc/decrement
                            // nothing to see here
                            continue;
                        } else if (access1.is(DECL)) {
                            // unused variable
                            ASTNode toRemove = getNodeWithoutSideEffectsToRemove(access1);
                            if (toRemove != null) {
                                ctx.getRefactorings().remove(toRemove);
                                continue;
                            }
                            // TODO JNR keep side effect assignments, i.e. only remove constants
                            continue;
                        } else if (access1.is(WRITE)) {
                            // dead store (no reads)
                            ASTNode toRemove = getNodeWithoutSideEffectsToRemove(access1);
                            if (toRemove != null) {
                                ctx.getRefactorings().remove(toRemove);
                                continue;
                            }
                            // TODO JNR keep side effect assignments, i.e. remove constants
                            continue;
                        } else {
                            throw new NotImplementedException(node, "Unknown access type: " + access1.getAccessType());
                        }
                    }

                    VariableAccess access2 = it.next();
                    if (!access2.is(READ)) {
                        // dead store (overwritten before read)
                        ASTNode toRemove = getNodeWithoutSideEffectsToRemove(access1);
                        if (toRemove != null) {
                            ctx.getRefactorings().remove(toRemove);
                            continue;
                        }
                        // TODO JNR keep side effect assignments, i.e. remove constants
                    }

                    Statement stmt2 = access2.getStatement();
                    boolean canReduceScopeOfVariable = canReduceScopeOfVariable((Block) node, stmt1, stmt2);
                    if (canReduceScopeOfVariable) {
                        // there are statements in between, reduce scope of variable
                        ASTBuilder b = ctx.getASTBuilder();
                        ctx.getRefactorings().insertBefore(b.move(stmt1), stmt2);
                    }
                    // TODO JNR do not return here
                    return;
                } else if (access1.is(DECL)) {
                    if (!it.hasNext()) {
                        // dead variable (never used)
                        ASTNode toRemove = getNodeWithoutSideEffectsToRemove(access1);
                        if (toRemove != null) {
                            ctx.getRefactorings().remove(toRemove);
                            continue;
                        }
                        // TODO JNR keep side effect assignments, i.e. remove constants
                    }
                } else {
                    // TODO JNR
                    // throw new NotImplementedException(node);
                }
            }
        }
    }

    private ASTNode getNodeWithoutSideEffectsToRemove(VariableAccess access) {
        Name varName = access.getVariableName();
        if (access.is(DECL)) {
            ASTNode ancestor = getAncestor(varName, VariableDeclarationFragment.class, SingleVariableDeclaration.class);
            if (ancestor instanceof VariableDeclarationFragment) {
                VariableDeclarationFragment vdf = (VariableDeclarationFragment) ancestor;
                Expression init = vdf.getInitializer();
                if (init == null || isConstant(init)) {
                    ASTNode parent = vdf.getParent();
                    if (parent instanceof VariableDeclarationStatement) {
                        VariableDeclarationStatement vds = (VariableDeclarationStatement) parent;
                        return vds.fragments().size() == 1 ? vds : vdf;
                    } else if (parent instanceof FieldDeclaration) {
                        FieldDeclaration fd = (FieldDeclaration) parent;
                        return fd.fragments().size() == 1 ? fd : vdf;
                    }
                }
            } else if (ancestor instanceof SingleVariableDeclaration) {
                SingleVariableDeclaration svd = (SingleVariableDeclaration) ancestor;
                Expression init = svd.getInitializer();
                if (init == null || isConstant(init)) {
                    return svd.getParent();
                }
            }
        } else if (access.is(WRITE)) {
            if (access.is(READ)) {
                // this a pre/post inc/decrement
                return getAncestor(varName, Statement.class);
            }
            Assignment as = getAncestor(varName, Assignment.class);
            if (isConstant(as.getRightHandSide())) {
                return getAncestor(as, Statement.class);
            }
        }
        return null;
    }

    private boolean canReduceScopeOfVariable(Block node, Statement stmt1, Statement stmt2) {
        List<Statement> stmts = asList(node);
        int idx1 = stmts.indexOf(stmt1);
        int idx2 = stmts.indexOf(stmt2);
        return idx1 != -1 && idx2 != -1 && idx2 - idx1 > 1;
    }

    private List<VariableAccess> getVariablesWithScope(ASTNode node) {
        List<VariableAccess> results = new ArrayList<VariableAccess>();
        for (Entry<VariableName, List<VariableAccess>> entry : allVariableAccesses.entrySet()) {
            for (VariableAccess varAccess : entry.getValue()) {
                if (isSameScope(node, varAccess.getScope())) {
                    results.add(varAccess);
                }
            }
        }
        return results;
    }

    private boolean isSameScope(ASTNode node1, ASTNode node2) {
        return isSameLevel(node1, node2) || isParentOf(node1, node2);
    }

    private boolean isSameLevel(ASTNode node1, ASTNode node2) {
        return node1.equals(node2);
    }

    private boolean isParentOf(ASTNode node1, ASTNode node2) {
        ASTNode parent = node2.getParent();
        while (parent != null) {
            if (node1.equals(parent)) {
                return true;
            }
            parent = parent.getParent();
        }
        return false;
    }

    @Override
    public boolean visit(SimpleName node) {
        findVariableAccesses(node);
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(QualifiedName node) {
        findVariableAccesses(node);
        return VISIT_SUBTREE;
    }

    private void findVariableAccesses(Name node) {
        final Pair<Integer, ASTNode> accessTypeAndScope = getAccessTypeAndScope(node);
        if (accessTypeAndScope != NULL_PAIR) {
            final VariableName varName = new VariableName(node);
            List<VariableAccess> list = this.allVariableAccesses.get(varName);
            if (list == null) {
                list = new ArrayList<VariableAccess>();
                this.allVariableAccesses.put(varName, list);
            }
            list.add(new VariableAccess(node, accessTypeAndScope.getFirst(), accessTypeAndScope.getSecond()));
        }
    }

    private Pair<Integer, ASTNode> getAccessTypeAndScope(ASTNode node) {
        final ASTNode parent = node.getParent();
        switch (parent.getNodeType()) {
        case BLOCK:
        case ENHANCED_FOR_STATEMENT:
        case EXPRESSION_STATEMENT:
        case FOR_STATEMENT:
        case SIMPLE_NAME:
        case WHILE_STATEMENT:
            return getAccessTypeAndScope(parent);

        case QUALIFIED_NAME:
            if (node.equals(((QualifiedName) parent).getQualifier())) {
                return NULL_PAIR;
            }
            return getAccessTypeAndScope(parent);

        case SINGLE_VARIABLE_DECLARATION:
            final SingleVariableDeclaration sVar = (SingleVariableDeclaration) parent;
            return Pair.of(sVar.getInitializer() != null ? WRITE | DECL : DECL, getScope(sVar.getParent()));

        case VARIABLE_DECLARATION_FRAGMENT:
            final VariableDeclarationFragment varDecl = (VariableDeclarationFragment) parent;
            return Pair.of(varDecl.getInitializer() != null ? WRITE | DECL : DECL, getScope(varDecl));

        case ASSIGNMENT:
            return Pair.of(WRITE, getScope(parent.getParent()));

        case ARRAY_ACCESS:
        case ARRAY_CREATION:
        case ARRAY_INITIALIZER:
        case CAST_EXPRESSION:
        case CONDITIONAL_EXPRESSION:
        case FIELD_ACCESS:
        case IF_STATEMENT:
        case INFIX_EXPRESSION:
        case INSTANCEOF_EXPRESSION:
        case RETURN_STATEMENT:
        case SWITCH_STATEMENT:
        case SYNCHRONIZED_STATEMENT:
        case THROW_STATEMENT:
            return Pair.of(READ, getScope(parent.getParent()));

        case METHOD_INVOCATION:
            if (node.equals(((MethodInvocation) parent).getName())) {
                return NULL_PAIR;
            }
            return Pair.of(READ, getScope(parent.getParent()));

        case SUPER_METHOD_INVOCATION:
            if (node.equals(((SuperMethodInvocation) parent).getName())) {
                return NULL_PAIR;
            }
            return Pair.of(READ, getScope(parent.getParent()));

        case PREFIX_EXPRESSION:
            PrefixExpression pr = (PrefixExpression) parent;
            Operator op = pr.getOperator();
            int accessType = DECREMENT.equals(op) || INCREMENT.equals(op) ? WRITE : READ;
            return Pair.of(accessType, getScope(parent.getParent()));

        case POSTFIX_EXPRESSION:
            return Pair.of(READ | WRITE, getScope(parent.getParent()));

        case IMPORT_DECLARATION:
        case METHOD_DECLARATION:
        case PACKAGE_DECLARATION:
        case ENUM_DECLARATION:
        case ENUM_CONSTANT_DECLARATION:
        case ARRAY_TYPE:
        case MEMBER_REF:
        case METHOD_REF:
        case PARAMETERIZED_TYPE:
        case PRIMITIVE_TYPE:
        case QUALIFIED_TYPE:
        case SIMPLE_TYPE:
        case SWITCH_CASE:
        case TAG_ELEMENT:
        case THIS_EXPRESSION:
        case TRY_STATEMENT: // FIXME JNR  this is wrong, but I am getting tired now
        case TYPE_DECLARATION:
        case TYPE_PARAMETER:
        case UNION_TYPE:
        case WILDCARD_TYPE:
        // ignore annotations
        case MARKER_ANNOTATION:
        case NORMAL_ANNOTATION:
        case SINGLE_MEMBER_ANNOTATION:
        // class name: not a variable
        case CLASS_INSTANCE_CREATION:
        case CONSTRUCTOR_INVOCATION:
        case SUPER_CONSTRUCTOR_INVOCATION:
            return NULL_PAIR;

        default:
            throw new NotImplementedException(parent);
        }
    }

    private ASTNode getScope(ASTNode node) {
        switch (node.getNodeType()) {
        case ANONYMOUS_CLASS_DECLARATION:
        case BLOCK:
        case CATCH_CLAUSE:
        case ENHANCED_FOR_STATEMENT:
        case ENUM_DECLARATION:
        case FOR_STATEMENT:
        case IF_STATEMENT:
        case METHOD_DECLARATION:
        case SWITCH_STATEMENT:
        case TYPE_DECLARATION:
        case WHILE_STATEMENT:
            return node;
        case ENUM_CONSTANT_DECLARATION:
        case FIELD_DECLARATION:
            return getScope(node.getParent());
        default:
            if (node instanceof Expression
                    || node instanceof Statement
                    || node instanceof VariableDeclaration) {
                return getScope(node.getParent());
            }
            throw new NotImplementedException(node);
        }
    }
}
