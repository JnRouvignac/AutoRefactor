/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.util.IllegalStateException;

/**
 * Collects code paths into the CFG.
 */
public class CodePathCollector {

    private final List<List<CFGBasicBlock>> results = new ArrayList<List<CFGBasicBlock>>();
    private final Deque<CFGBasicBlock> stack = new LinkedList<CFGBasicBlock>();

    /**
     * Returns the collected code paths.
     *
     * @param entryBlock the entry block of the CFG
     * @return the collected code paths
     */
    public Collection<List<CFGBasicBlock>> getPaths(CFGBasicBlock entryBlock) {
        collectPathes(entryBlock);
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
                throw new IllegalStateException(block.getNode(),
                        "Path should have ended with an exit block: " + stack);
            }
        } finally {
            stack.pop();
        }
    }
}
