package org.autorefactor.cfg;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;

/**
 * Holds all the basic blocks that can potentially throw. 
 */
class ThrowerBlocks {

	private final Map<CFGBasicBlock, ITypeBinding[]> potentialThrows =
			new HashMap<CFGBasicBlock, ITypeBinding[]>();

	public void addThrow(CFGBasicBlock basicBlock,
			ITypeBinding... exceptionTypes) {
		if (exceptionTypes != null && exceptionTypes.length > 0) {
			potentialThrows.put(basicBlock, exceptionTypes);
		}
	}

	public void addThrow(Expression e, ITypeBinding[] newExceptions) {
		// TODO JNR remove
	}

	public List<CFGBasicBlock> selectBlocksThrowing(ITypeBinding exceptionTypeToFind) {
		final List<CFGBasicBlock> results = new LinkedList<CFGBasicBlock>();
		for (Entry<CFGBasicBlock, ITypeBinding[]> entry : potentialThrows
				.entrySet()) {
			for (ITypeBinding thrownType : entry.getValue()) {
				if (exceptionTypeToFind.equals(thrownType)) {
					results.add(entry.getKey());
				}
			}
		}
		return results;
	}

	public List<CFGBasicBlock> selectBlocksThrowingOtherThan(
			Set<ITypeBinding> exceptionTypesToReject) {
		final List<CFGBasicBlock> results = new LinkedList<CFGBasicBlock>();
		for (Entry<CFGBasicBlock, ITypeBinding[]> entry : potentialThrows.entrySet()) {
			final Set<ITypeBinding> bindings = getNonMatching(
					exceptionTypesToReject, entry.getValue());
			if (bindings != null) {
				results.add(entry.getKey());
			}
		}
		return results;
	}

	private Set<ITypeBinding> getNonMatching(Set<ITypeBinding> exceptionTypes,
			ITypeBinding[] thrownTypes) {
		final Set<ITypeBinding> results = new HashSet<ITypeBinding>();
		for (ITypeBinding thrownType : thrownTypes) {
			if (!exceptionTypes.contains(thrownType)) {
				results.add(thrownType);
			}
		}
		return results;
	}

	@Override
	public String toString() {
		return "potentialThrows=" + potentialThrows;
	}

}
