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
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.IllegalArgumentException;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.util.Utils.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

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

    private final Map<VariableName, List<VariableAccess>> allVariableAccesses =
            new HashMap<VariableName, List<VariableAccess>>();
    private static final Pair<Integer, ASTNode> NULL_PAIR = Pair.of(0, null);

    @Override
    public void postVisit(ASTNode node) {
        if (node.getNodeType() != BLOCK) {
            return;
        }

        List<VariableAccess> variableAccesses = getVariablesWithScope(node);
        List<VariableAccess> iVarAccesses = new ArrayList<VariableAccess>();
        if (!variableAccesses.isEmpty()) {
            for (VariableAccess access : variableAccesses) {
                Name varName = access.variableName;
                if (varName instanceof SimpleName) {
                    SimpleName name = (SimpleName) varName;
                    // TODO JNR remove this loop and this test
                    if ("i".equals(name.getIdentifier())) {
                        iVarAccesses.add(access);
                    }
                }
            }
        }

        if (!iVarAccesses.isEmpty()) {
            for (ListIterator<VariableAccess> it = iVarAccesses.listIterator(); it.hasNext();) {
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
            VariableDeclarationFragment vdf = getAncestor(varName, VariableDeclarationFragment.class);
            Expression init = vdf.getInitializer();
            if (init != null && isConstant(init)) {
                VariableDeclarationStatement vds = (VariableDeclarationStatement) vdf.getParent();
                return vds.fragments().size() == 1 ? vds : vdf;
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
        case INFIX_EXPRESSION:
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
            return getAccessTypeAndScope(sVar.getParent());

        case VARIABLE_DECLARATION_FRAGMENT:
            final VariableDeclarationFragment varDecl = (VariableDeclarationFragment) parent;
            return Pair.of(varDecl.getInitializer() != null ? WRITE | DECL : DECL, getScope(varDecl));

        case ASSIGNMENT:
            return Pair.of(WRITE, getScope(parent.getParent()));

        case METHOD_INVOCATION:
            if (node.equals(((MethodInvocation) parent).getName())) {
                return NULL_PAIR;
            }
            return Pair.of(READ, getScope(parent.getParent()));

        case POSTFIX_EXPRESSION:
            return Pair.of(READ | WRITE, getScope(parent.getParent()));

        case IMPORT_DECLARATION:
        case METHOD_DECLARATION:
        case PACKAGE_DECLARATION:
        case TYPE_DECLARATION:
        case ARRAY_TYPE:
        case PARAMETERIZED_TYPE:
        case PRIMITIVE_TYPE:
        case QUALIFIED_TYPE:
        case SIMPLE_TYPE:
        case UNION_TYPE:
        case WILDCARD_TYPE:
            return NULL_PAIR;

        default:
            throw new NotImplementedException(parent);
        }
    }

    private ASTNode getScope(ASTNode node) {
        switch (node.getNodeType()) {
        case BLOCK:
        case ENHANCED_FOR_STATEMENT:
        case FOR_STATEMENT:
        case IF_STATEMENT:
        case WHILE_STATEMENT:
            return node;
        default:
            if (node instanceof Expression
                    || node instanceof Statement
                    || node instanceof VariableDeclaration) {
                return getScope(node.getParent());
            }
            throw new NotImplementedException(node);
        }
    }

    @Override
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);

        for (Entry<VariableName, List<VariableAccess>> entry : this.allVariableAccesses.entrySet()) {
            final List<VariableAccess> variableAccesses = entry.getValue();
            if (canReduceVariableScope(variableAccesses)) {
                final VariableAccess varDecl = variableAccesses.get(0);
                remove(varDecl.getVariableName());

                for (VariableAccess varAccess : variableAccesses) {
                    if (varAccess.getAccessType() == WRITE) {
                        replace(varDecl, varAccess);
                    } // TODO JNR if (varAccess.getAccessType() & WRITE) {
                }
            }
        }

        // TODO JNR remove writes when there are no reads after
        // TODO JNR remove double writes when there are no reads after

        return this.ctx.getRefactorings();
    }

    private void replace(VariableAccess varDecl, VariableAccess varAccess) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final AST ast = b.getAST();
        final ASTNode scope = varAccess.getScope();
        final Name varName = varAccess.getVariableName();
        final Type varType = getType(varDecl.getVariableName().getParent());
        if (scope instanceof Block) {
            final List<Statement> stmts = statements((Block) scope);
            for (int i = 0; i < stmts.size(); i++) {
                final Statement stmt = stmts.get(i);
                final Expression parentExpr = getAncestor(varName, Expression.class);  // FIXME i=0
                final Statement parentStmt = getAncestor(parentExpr, Statement.class); // FIXME i=0
                if (stmt.equals(parentStmt)) {
                    final VariableDeclarationFragment vdf = getVariableDeclarationFragment(parentExpr, varName);
                    final VariableDeclarationStatement vds = ast.newVariableDeclarationStatement(vdf);
                    vds.setType(varType);
                    this.ctx.getRefactorings().replace(stmt, vds);
                    break;
                }
            }
        } else if (scope instanceof EnhancedForStatement) {
            final EnhancedForStatement efs = (EnhancedForStatement) scope;
            final EnhancedForStatement newEfs = b.copy(efs);
            newEfs.setParameter(b.copy(efs.getParameter()));
            newEfs.setExpression(b.copy(efs.getExpression()));
            final Statement parentStmt = getAncestor(varName, Statement.class);
            if (equalNotNull(efs.getBody(), parentStmt)) {
                newEfs.setBody(copy(efs.getBody(), varName));
            }
            this.ctx.getRefactorings().replace(efs, newEfs);
        } else if (scope instanceof ForStatement) {
            final ForStatement fs = (ForStatement) scope;
            final ForStatement newFs = b.copy(fs);
            final List<Expression> initializers = initializers(newFs);
            if (initializers.size() == 1) {
                final Expression init = initializers.remove(0);
                final VariableDeclarationFragment vdf = getVariableDeclarationFragment(init, varName);
                final VariableDeclarationExpression vde = ast.newVariableDeclarationExpression(vdf);
                vde.setType(varType);
                initializers.add(vde);
                this.ctx.getRefactorings().replace(fs, newFs);
                // TODO JNR
                // if (equalNotNull(fs.getBody(), parentStmt)) {
                // newFs.setBody(copy(fs.getBody()));
                // }
            } else {
                throw new NotImplementedException(scope, "for more than one initializer in for loop.");
            }
        } else if (scope instanceof WhileStatement) {
            final WhileStatement ws = (WhileStatement) scope;
            final WhileStatement newWs = ast.newWhileStatement();
            newWs.setExpression(b.copy(ws.getExpression()));
            final Statement parentStmt = getAncestor(varName, Statement.class);
            if (equalNotNull(ws.getBody(), parentStmt)) {
                newWs.setBody(copy(ws.getBody(), varName));
            }
            this.ctx.getRefactorings().replace(ws, newWs);
        } else if (scope instanceof IfStatement) {
            final IfStatement is = (IfStatement) scope;
            final IfStatement newIs = ast.newIfStatement();
            newIs.setExpression(b.copy(is.getExpression()));
            final Statement parentStmt = getAncestor(varName, Statement.class);
            if (equalNotNull(is.getThenStatement(), parentStmt)) {
                newIs.setThenStatement(copy(is.getThenStatement(), varName));
                if (is.getElseStatement() != null) {
                    newIs.setElseStatement(b.copy(is.getElseStatement()));
                }
                this.ctx.getRefactorings().replace(is, newIs);
            } else if (equalNotNull(is.getElseStatement(), parentStmt)) {
                if (is.getThenStatement() != null) {
                    newIs.setThenStatement(b.copy(is.getThenStatement()));
                }
                newIs.setElseStatement(copy(is.getElseStatement(), varName));
                this.ctx.getRefactorings().replace(is, newIs);
            } else {
                throw new IllegalStateException(is,
                        "Parent statement should be inside the then or else statement of this if statement: " + is);
            }
        } else {
            throw new NotImplementedException(scope);
        }
    }

    private Block copy(Statement stmtToCopy, Name varName) {
        if (stmtToCopy != null && !(stmtToCopy instanceof Block)) {
            final Block b = this.ctx.getAST().newBlock();
            final Assignment a = asExpression(stmtToCopy, Assignment.class);
            if (a != null) {
                final VariableDeclarationFragment vdf = getVariableDeclarationFragment(a, varName);
                statements(b).add(this.ctx.getAST().newVariableDeclarationStatement(vdf));
            } else {
                throw new NotImplementedException(stmtToCopy);
            }
            return b;
        }
        // We should never come here if we had a Block statement, see the replace() method
        throw new NotImplementedException(stmtToCopy);
    }

    private Type getType(ASTNode node) {
        if (node instanceof VariableDeclarationStatement) {
            final VariableDeclarationStatement vds = (VariableDeclarationStatement) node;
            return this.ctx.getASTBuilder().copy(vds.getType());
        }
        return getType(node.getParent());
    }

    private VariableDeclarationFragment getVariableDeclarationFragment(Expression exprToReplace, Name varName) {
        if (exprToReplace instanceof Assignment) {
            final Assignment a = (Assignment) exprToReplace;
            if (a.getLeftHandSide() instanceof SimpleName) {
                final SimpleName sn = (SimpleName) a.getLeftHandSide();
                if (sn.getFullyQualifiedName().equals(varName.getFullyQualifiedName())) {
                    final ASTBuilder b = this.ctx.getASTBuilder();
                    final VariableDeclarationFragment vdf = b.getAST().newVariableDeclarationFragment();
                    vdf.setInitializer(b.copy(a.getRightHandSide()));
                    vdf.setName(b.copy(sn));
                    return vdf;
                }
            }
            throw new NotImplementedException(a.getLeftHandSide());
        }
        throw new NotImplementedException(exprToReplace);
    }

    private void remove(ASTNode node) {
        if (node instanceof VariableDeclarationFragment) {
            this.ctx.getRefactorings().remove(node.getParent());
        } else {
            remove(node.getParent());
        }
    }

    private boolean canReduceVariableScope(final List<VariableAccess> variableAccesses) {
        final VariableAccess varDecl = variableAccesses.get(0);

        VariableAccess lastWrite = null;
        for (VariableAccess varAccess : variableAccesses) {
            if (varAccess.getAccessType() == WRITE) {
                // is only write
                lastWrite = varAccess;
            } else if ((varAccess.getAccessType() & READ) != 0) {
                // is read
                if (lastWrite != null
                        && !isReadDominatedByWriteInScopeMoreReducedThanVariableScope(
                                varAccess.getScope(), lastWrite.getScope(), varDecl.getScope())) {
                    // TODO JNR return sublist of reduceable scope
                    return false;
                }
            }
        }
        return true;
    }

    private boolean isReadDominatedByWriteInScopeMoreReducedThanVariableScope(
            ASTNode readScope, ASTNode writeScope, ASTNode varScope) {
        if (isSameScope(varScope, readScope) || isSameScope(varScope, writeScope)) {
            return false;
        }
        if (isSameScope(readScope, writeScope)) {
            return true;
        }
        return isReadDominatedByWriteInScopeMoreReducedThanVariableScope(
                readScope.getParent(), writeScope, varScope);
    }
}
