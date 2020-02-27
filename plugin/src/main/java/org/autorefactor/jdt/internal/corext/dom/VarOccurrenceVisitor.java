package org.autorefactor.jdt.internal.corext.dom;

import static org.eclipse.jdt.core.dom.IBinding.VARIABLE;

import java.util.Set;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.SimpleName;

/**
 * The variable occurrence visitor. By default, a variable is supposed to be used.
 */
public class VarOccurrenceVisitor extends InterruptibleVisitor {
    private final Set<IVariableBinding> localVarIds;
    private boolean varUsed;
    private ASTNode startNode;
    private final boolean includeInnerScopes;

    /**
     * Returns true if at least one variable is used.
     *
     * @return True if at least one variable is used
     */
    public boolean isVarUsed() {
        return varUsed;
    }

    /**
     * The constructor.
     *
     * @param localVarIds The ids of the variable to search
     * @param includeInnerScopes True if the sub blocks should be analyzed
     */
    public VarOccurrenceVisitor(final Set<IVariableBinding> localVarIds, final boolean includeInnerScopes) {
        this.localVarIds= localVarIds;
        this.includeInnerScopes= includeInnerScopes;
    }

    @Override
    public void visitNode(final ASTNode startNode) {
        this.startNode= startNode;
        super.visitNode(this.startNode);
    }

    @Override
    public boolean visit(final SimpleName node) {
        IBinding binding= node.resolveBinding();

        if (binding == null || binding.getKind() == VARIABLE && localVarIds.contains(binding)) {
            varUsed= true;
            return interruptVisit();
        }

        return true;
    }

    @Override
    public boolean visit(final Block node) {
        return startNode == node || includeInnerScopes;
    }
}
