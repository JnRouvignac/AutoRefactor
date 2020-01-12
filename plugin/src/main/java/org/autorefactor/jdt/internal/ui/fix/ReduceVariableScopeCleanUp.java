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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.util.IllegalArgumentException;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/**
 * TODO JNR can we also transform singular fields into local variables?
 *
 * @see {@link #getDescription()}
 */
public class ReduceVariableScopeCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Reduce scope of variable"; //$NON-NLS-1$
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Reduces the scope of local variables."; //$NON-NLS-1$
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the reading and debugging cost."; //$NON-NLS-1$
    }

    private static final int DECL= 1 << 0;
    private static final int READ= 1 << 1;
    private static final int WRITE= 1 << 2;

    private static final class VariableName {
        private final Name name;

        public VariableName(final Name name) {
            this.name= name;
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj instanceof VariableName) {
                final VariableName other= (VariableName) obj;
                if (this.name instanceof SimpleName && other.name instanceof SimpleName) {
                    return ASTNodes.isEqual((SimpleName) this.name, (SimpleName) other.name);
                }
                // if (this.name instanceof QualifiedName
                // && other.name instanceof QualifiedName) {
                // throw new IllegalStateException();
                // }
            }
            // Return false;
            throw new NotImplementedException(name, name);
        }

        @Override
        public int hashCode() {
            if (this.name instanceof SimpleName) {
                return ((SimpleName) this.name).getIdentifier().hashCode();
            }
            // if (this.name instanceof QualifiedName) {
            // throw new IllegalStateException();
            // }
            throw new NotImplementedException(name, name);
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

        public VariableAccess(final Name variableName, final int accessType, final ASTNode scope) {
            if (accessType == 0) {
                throw new IllegalArgumentException(null, "accessType must not be null"); //$NON-NLS-1$
            }
            this.variableName= variableName;
            this.accessType= accessType;
            this.scope= scope;
        }

        public Name getVariableName() {
            return variableName;
        }

        public int getAccessType() {
            return accessType;
        }

        public ASTNode getScope() {
            return scope;
        }

        @Override
        public String toString() {
            final StringBuilder sb= new StringBuilder();
            if ((this.accessType & DECL) != 0) {
                sb.append("DECL"); //$NON-NLS-1$
            }
            if ((this.accessType & READ) != 0) {
                if (sb.length() > 0) {
                    sb.append(" | "); //$NON-NLS-1$
                }
                sb.append("READ"); //$NON-NLS-1$
            }
            if ((this.accessType & WRITE) != 0) {
                if (sb.length() > 0) {
                    sb.append(" | "); //$NON-NLS-1$
                }
                sb.append("WRITE"); //$NON-NLS-1$
            }
            sb.append("\n"); //$NON-NLS-1$
            sb.append(scope);
            return sb.toString();
        }
    }

    private final Map<VariableName, List<VariableAccess>> allVariableAccesses= new HashMap<>();
    private static final Pair<Integer, ASTNode> NULL_PAIR= Pair.of(0, null);

    @Override
    public boolean visit(final SimpleName node) {
        findVariableAccesses(node);
        return true;
    }

    @Override
    public boolean visit(final QualifiedName node) {
        findVariableAccesses(node);
        return true;
    }

    private void findVariableAccesses(final Name node) {
        final Pair<Integer, ASTNode> accessTypeAndScope= getAccessTypeAndScope(node);
        if (accessTypeAndScope.getFirst().intValue() != 0) {
            final VariableName varName= new VariableName(node);
            List<VariableAccess> list= this.allVariableAccesses.get(varName);
            if (list == null) {
                list= new ArrayList<>();
                this.allVariableAccesses.put(varName, list);
            }
            if (list.isEmpty() || !Utils.getLast(list).getScope().equals(accessTypeAndScope.getSecond())) {
                // Only keep first write in scope
                list.add(new VariableAccess(node, accessTypeAndScope.getFirst(), accessTypeAndScope.getSecond()));
            }
        }
    }

    private Pair<Integer, ASTNode> getAccessTypeAndScope(final ASTNode node) {
        final ASTNode parent= node.getParent();
        if (parent instanceof Block || parent instanceof InfixExpression || parent instanceof EnhancedForStatement
                || parent instanceof ExpressionStatement || parent instanceof ForStatement || parent instanceof Name
                || parent instanceof WhileStatement) {
            return getAccessTypeAndScope(parent);
        }
        if (parent instanceof ImportDeclaration || parent instanceof MethodDeclaration
                || parent instanceof MethodInvocation || parent instanceof PackageDeclaration || parent instanceof Type
                || parent instanceof TypeDeclaration) {
            return NULL_PAIR;
        }
        if (parent instanceof SingleVariableDeclaration) {
            final SingleVariableDeclaration var= (SingleVariableDeclaration) parent;
            return getAccessTypeAndScope(var.getParent());
        }
        if (parent instanceof VariableDeclarationFragment) {
            final VariableDeclarationFragment var= (VariableDeclarationFragment) parent;
            return Pair.of(var.getInitializer() != null ? WRITE | DECL : DECL, getScope(var));
        }
        if (parent instanceof Assignment) {
            return Pair.of(WRITE, getScope(parent.getParent()));
        }
        if (parent instanceof InfixExpression) {
            return Pair.of(READ, getScope(parent.getParent()));
        }
        if (parent instanceof PostfixExpression) {
            return Pair.of(READ | WRITE, getScope(parent.getParent()));
        }
        throw new NotImplementedException(parent);
    }

    private ASTNode getScope(final ASTNode node) {
        if (node instanceof Block || node instanceof EnhancedForStatement || node instanceof ForStatement
                || node instanceof IfStatement || node instanceof WhileStatement) {
            return node;
        }
        if (node instanceof Expression || node instanceof Statement || node instanceof VariableDeclaration) {
            return getScope(node.getParent());
        }
        throw new NotImplementedException(node);
    }

    @Override
    public Refactorings getRefactorings(final CompilationUnit astRoot) {
        astRoot.accept(this);

        for (Entry<VariableName, List<VariableAccess>> entry : this.allVariableAccesses.entrySet()) {
            final List<VariableAccess> variableAccesses= entry.getValue();
            if (canReduceVariableScope(variableAccesses)) {
                final VariableAccess varDecl= variableAccesses.get(0);
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

    private void replace(final VariableAccess varDecl, final VariableAccess varAccess) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        final AST ast= b.getAST();
        final ASTNode scope= varAccess.getScope();
        final Name varName= varAccess.getVariableName();
        final Type varType= getType(varDecl.getVariableName().getParent());
        if (scope instanceof Block) {
            final List<Statement> statements= ASTNodes.statements((Block) scope);
            for (int i= 0; i < statements.size(); i++) {
                final Statement statement= statements.get(i);
                final Expression parentExpression= ASTNodes.getAncestor(varName, Expression.class); // FIXME i=0
                final Statement parentStatement= ASTNodes.getAncestor(parentExpression, Statement.class); // FIXME i=0
                if (statement.equals(parentStatement)) {
                    final VariableDeclarationFragment vdf= getVariableDeclarationFragment(parentExpression, varName);
                    final VariableDeclarationStatement vds= ast.newVariableDeclarationStatement(vdf);
                    vds.setType(varType);
                    this.ctx.getRefactorings().replace(statement, vds);
                    break;
                }
            }
        } else if (scope instanceof EnhancedForStatement) {
            final EnhancedForStatement efs= (EnhancedForStatement) scope;
            final EnhancedForStatement newEfs= b.createCopyTarget(efs);
            newEfs.setParameter(b.createCopyTarget(efs.getParameter()));
            newEfs.setExpression(b.createCopyTarget(efs.getExpression()));
            final Statement parentStatement= ASTNodes.getAncestor(varName, Statement.class);
            if (Utils.equalNotNull(efs.getBody(), parentStatement)) {
                newEfs.setBody(copy(efs.getBody(), varName));
            }
            this.ctx.getRefactorings().replace(efs, newEfs);
        } else if (scope instanceof ForStatement) {
            final ForStatement fs= (ForStatement) scope;
            final ForStatement newFs= b.createCopyTarget(fs);
            final List<Expression> initializers= ASTNodes.initializers(newFs);
            if (initializers.size() != 1) {
                throw new NotImplementedException(scope, "for more than one initializer in for loop."); //$NON-NLS-1$
            }
            final Expression init= initializers.remove(0);
            final VariableDeclarationFragment vdf= getVariableDeclarationFragment(init, varName);
            final VariableDeclarationExpression vde= ast.newVariableDeclarationExpression(vdf);
            vde.setType(varType);
            initializers.add(vde);
            this.ctx.getRefactorings().replace(fs, newFs);
            // TODO JNR
            // if (equalNotNull(fs.getBody(), parentStatement)) {
            // newFs.setBody(copy(fs.getBody()));
            // }
        } else if (scope instanceof WhileStatement) {
            final WhileStatement ws= (WhileStatement) scope;
            final WhileStatement newWs= ast.newWhileStatement();
            newWs.setExpression(b.createCopyTarget(ws.getExpression()));
            final Statement parentStatement= ASTNodes.getAncestor(varName, Statement.class);
            if (Utils.equalNotNull(ws.getBody(), parentStatement)) {
                newWs.setBody(copy(ws.getBody(), varName));
            }
            this.ctx.getRefactorings().replace(ws, newWs);
        } else if (scope instanceof IfStatement) {
            final IfStatement is= (IfStatement) scope;
            final IfStatement newIs= ast.newIfStatement();
            newIs.setExpression(b.createCopyTarget(is.getExpression()));
            final Statement parentStatement= ASTNodes.getAncestor(varName, Statement.class);
            if (Utils.equalNotNull(is.getThenStatement(), parentStatement)) {
                newIs.setThenStatement(copy(is.getThenStatement(), varName));
                if (is.getElseStatement() != null) {
                    newIs.setElseStatement(b.createCopyTarget(is.getElseStatement()));
                }
            } else if (Utils.equalNotNull(is.getElseStatement(), parentStatement)) {
                if (is.getThenStatement() != null) {
                    newIs.setThenStatement(b.createCopyTarget(is.getThenStatement()));
                }
                newIs.setElseStatement(copy(is.getElseStatement(), varName));
            } else {
                throw new IllegalStateException(is,
                        "Parent statement should be inside the then or else statement of this if statement: " + is); //$NON-NLS-1$
            }
            this.ctx.getRefactorings().replace(is, newIs);
        } else {
            throw new NotImplementedException(scope);
        }
    }

    private Block copy(final Statement stmtToCopy, final Name varName) {
        if (stmtToCopy != null && !(stmtToCopy instanceof Block)) {
            final Block b= this.ctx.getAST().newBlock();
            final Assignment a= ASTNodes.asExpression(stmtToCopy, Assignment.class);
            if (a == null) {
                throw new NotImplementedException(stmtToCopy);
            }
            final VariableDeclarationFragment vdf= getVariableDeclarationFragment(a, varName);
            ASTNodes.statements(b).add(this.ctx.getAST().newVariableDeclarationStatement(vdf));
            return b;
        }
        // We should never come here if we had a Block statement, see the replace()
        // method
        throw new NotImplementedException(stmtToCopy);
    }

    private Type getType(final ASTNode node) {
        if (node instanceof VariableDeclarationStatement) {
            final VariableDeclarationStatement vds= (VariableDeclarationStatement) node;
            return this.ctx.getASTBuilder().createCopyTarget(vds.getType());
        }

        return getType(node.getParent());
    }

    private VariableDeclarationFragment getVariableDeclarationFragment(final Expression exprToReplace, final Name varName) {
        if (exprToReplace instanceof Assignment) {
            final Assignment a= (Assignment) exprToReplace;
            if (a.getLeftHandSide() instanceof SimpleName) {
                final SimpleName sn= (SimpleName) a.getLeftHandSide();
                if (sn.getFullyQualifiedName().equals(varName.getFullyQualifiedName())) {
                    final ASTNodeFactory b= this.ctx.getASTBuilder();
                    final VariableDeclarationFragment vdf= b.getAST().newVariableDeclarationFragment();
                    vdf.setInitializer(b.createCopyTarget(a.getRightHandSide()));
                    vdf.setName(b.createCopyTarget(sn));
                    return vdf;
                }
            }
            throw new NotImplementedException(a.getLeftHandSide());
        }
        throw new NotImplementedException(exprToReplace);
    }

    private void remove(final ASTNode node) {
        if (node instanceof VariableDeclarationFragment) {
            this.ctx.getRefactorings().remove(node.getParent());
        } else {
            remove(node.getParent());
        }
    }

    private boolean canReduceVariableScope(final List<VariableAccess> variableAccesses) {
        final VariableAccess varDecl= variableAccesses.get(0);

        VariableAccess lastWrite= null;
        for (VariableAccess varAccess : variableAccesses) {
            if (varAccess.getAccessType() == WRITE) {
                // Is only write
                lastWrite= varAccess;
            } else if ((varAccess.getAccessType() & READ) != 0 && lastWrite != null
                    && !isReadDominatedByWriteInScopeMoreReducedThanVariableScope(varAccess.getScope(),
                            lastWrite.getScope(), varDecl.getScope())) {
                // TODO JNR return sublist of reduceable scope
                return false;
            }
        }

        return true;
    }

    private boolean isReadDominatedByWriteInScopeMoreReducedThanVariableScope(final ASTNode readScope, final ASTNode writeScope,
            final ASTNode varScope) {
        return !varScope.equals(readScope) && !varScope.equals(writeScope)
                && (readScope.equals(writeScope) || isReadDominatedByWriteInScopeMoreReducedThanVariableScope(
                        readScope.getParent(), writeScope, varScope));
    }
}
