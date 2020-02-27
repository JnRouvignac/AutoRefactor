package org.autorefactor.jdt.internal.corext.dom;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/**
 * Visitor collecting all definitions of any variable.
 */
public class VarDeclarationIdentifierVisitor extends ASTVisitor {
    private final Set<IVariableBinding> variableBindings= new HashSet<>();
    private final Set<String> variableNames= new HashSet<>();
    private final ASTNode startNode;
    private final boolean includeInnerScopes;

    /**
     * The constructor.
     *
     * @param startNode       the {@link ASTNode} which is the scope of the search
     * @param includeInnerScopes True if the sub blocks should be analyzed
     */
    public VarDeclarationIdentifierVisitor(final ASTNode startNode, final boolean includeInnerScopes) {
        this.startNode= startNode;
        this.includeInnerScopes= includeInnerScopes;
    }

    /**
     * Get the variable bindings.
     *
     * @return the variable bindings.
     */
    public Set<IVariableBinding> getVariableBindings() {
        return variableBindings;
    }

    /**
     * Get the variable names.
     *
     * @return the variable names.
     */
    public Set<String> getVariableNames() {
        return variableNames;
    }

    @Override
    public boolean visit(final SingleVariableDeclaration node) {
        variableBindings.add(node.resolveBinding());
        variableNames.add(node.getName().getIdentifier());
        return true;
    }

    @Override
    public boolean visit(final VariableDeclarationFragment node) {
        variableBindings.add(node.resolveBinding());
        variableNames.add(node.getName().getIdentifier());
        return true;
    }

    @Override
    public boolean visit(final Block node) {
        return startNode == node || includeInnerScopes;
    }
}
