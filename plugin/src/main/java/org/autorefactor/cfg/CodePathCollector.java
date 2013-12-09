package org.autorefactor.cfg;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public class CodePathCollector {

	private final List<List<CFGBasicBlock>> results = new ArrayList<List<CFGBasicBlock>>();
	private final Deque<CFGBasicBlock> stack = new LinkedList<CFGBasicBlock>();

	public Collection<List<CFGBasicBlock>> getPaths(CFGBasicBlock block) {
		collectPathes(block);
		return results;
	}

	private void collectPathes(CFGBasicBlock block) {
		if (block.isExitBlock()) {
			// this is the end of this path,
			// let's take a copy of the stack
			results.add(new ArrayList<CFGBasicBlock>(stack));
			return;
		}
		if (stack.contains(block)) {
			// cycle detected, let's stop it here
			return;
		}

		stack.push(block);
		try {
			boolean foundAtLeastOneEdge = false;
			for (Object obj : block.getOutgoingEdgesAndVariableAccesses()) {
				if (obj instanceof CFGEdge) {
					final CFGEdge edge = (CFGEdge) obj;
					collectPathes(edge.getTargetBlock());
					foundAtLeastOneEdge = true;
				}
			}
			if (!foundAtLeastOneEdge) {
				throw new IllegalStateException(
						"Path should have ended with an exit block: " + stack);
			}
		} finally {
			stack.pop();
		}
	}
}
