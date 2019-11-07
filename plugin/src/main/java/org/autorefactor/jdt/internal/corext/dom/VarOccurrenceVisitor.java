package org.autorefactor.jdt.internal.corext.dom;

import java.util.Set;

import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.SimpleName;

/**
 * The variable occurrence visitor.
 */
public class VarOccurrenceVisitor extends InterruptibleVisitor {
    private final Set<String> localVarIds;
    private boolean varUsed;

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
     */
    public VarOccurrenceVisitor(final Set<String> localVarIds) {
        this.localVarIds= localVarIds;
    }

    @Override
    public boolean visit(final SimpleName aVariable) {
        if (localVarIds.contains(aVariable.getIdentifier())) {
            varUsed= true;
            return interruptVisit();
        }

        return true;
    }

    @Override
    public boolean visit(final Block node) {
        return false;
    }
}
