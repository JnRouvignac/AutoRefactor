package org.autorefactor.jdt.internal.corext.dom;

import java.util.Set;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.SimpleName;

/**
 * The variable occurrence visitor.
 */
public class VarOccurrenceVisitor extends InterruptibleVisitor {
	private final Set<String> localVarIds;
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
	public VarOccurrenceVisitor(final Set<String> localVarIds, final boolean includeInnerScopes) {
		this.localVarIds= localVarIds;
		this.includeInnerScopes= includeInnerScopes;
	}

	@Override
	public void visitNode(final ASTNode startNode) {
		this.startNode= startNode;
		super.visitNode(this.startNode);
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
		return startNode == node || includeInnerScopes;
	}
}
