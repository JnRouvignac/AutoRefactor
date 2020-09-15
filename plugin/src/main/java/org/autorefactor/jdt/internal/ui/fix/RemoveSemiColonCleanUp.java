/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.jdt.internal.corext.dom.ASTComments;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.SourceLocation;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclarationStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.text.edits.TextEditGroup;

/**
 * See {@link #getDescription()} method.
 * <p>
 * TODO remove superfluous semi-colons in try-with-resources
 */
public class RemoveSemiColonCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.RemoveSemiColonCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.RemoveSemiColonCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.RemoveSemiColonCleanUp_reason;
	}

	@Override
	public boolean visit(final AnnotationTypeDeclaration node) {
		return visit((BodyDeclaration) node);
	}

	@Override
	public boolean visit(final EnumDeclaration node) {
		return visit((BodyDeclaration) node);
	}

	@Override
	public boolean visit(final FieldDeclaration node) {
		return visit((BodyDeclaration) node);
	}

	@Override
	public boolean visit(final Initializer node) {
		return visit((BodyDeclaration) node);
	}

	@Override
	public boolean visit(final MethodDeclaration node) {
		return visit((BodyDeclaration) node);
	}

	@Override
	public boolean visit(final TypeDeclaration node) {
		return visit((BodyDeclaration) node);
	}

	private boolean visit(final BodyDeclaration node) {
		BodyDeclaration nextSibling= ASTNodes.getNextSibling(node);
		ASTNode parent= node.getParent();

		if (nextSibling != null) {
			return maybeRemoveSuperfluousSemiColons(node, SourceLocation.getEndPosition(node), nextSibling.getStartPosition());
		}

		if (parent instanceof AbstractTypeDeclaration) {
			AbstractTypeDeclaration typeDecl= (AbstractTypeDeclaration) parent;
			return maybeRemoveSuperfluousSemiColons(node, SourceLocation.getEndPosition(node), SourceLocation.getEndPosition(typeDecl) - 1);
		}

		if (parent instanceof AnonymousClassDeclaration) {
			AnonymousClassDeclaration classDecl= (AnonymousClassDeclaration) parent;
			return maybeRemoveSuperfluousSemiColons(node, SourceLocation.getEndPosition(node), SourceLocation.getEndPosition(classDecl) - 1);
		}

		if (parent instanceof CompilationUnit) {
			CompilationUnit cu= (CompilationUnit) parent;
			return maybeRemoveSuperfluousSemiColons(node, SourceLocation.getEndPosition(node), SourceLocation.getEndPosition(cu) - 1);
		}

		if (parent instanceof TypeDeclarationStatement) {
			return true;
		}

		throw new NotImplementedException(node,
				"for a parent of type " + (parent != null ? parent.getClass().getSimpleName() : null)); //$NON-NLS-1$
	}

	private boolean maybeRemoveSuperfluousSemiColons(final ASTNode node, final int start, final int end) {
		if (end <= start) {
			return true;
		}

		boolean result= true;
		Map<String, SourceLocation> nonCommentsStrings= getNonCommentsStrings(node, start, end);

		for (Entry<String, SourceLocation> entry : nonCommentsStrings.entrySet()) {
			String s= entry.getKey();
			Matcher m= Pattern.compile("\\s*(;+)\\s*").matcher(s); //$NON-NLS-1$

			while (m.find()) {
				int startPos= entry.getValue().getStartPosition();
				SourceLocation toRemove= SourceLocation.fromPositions(startPos + m.start(1), startPos + m.end(1));
				TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveSemiColonCleanUp_description);
				cuRewrite.getASTRewrite().remove(toRemove);
				result= false;
			}
		}

		return result;
	}

	private Map<String, SourceLocation> getNonCommentsStrings(final ASTNode node, final int start, final int end) {
		List<Comment> comments= filterCommentsInRange(start, end, node.getRoot());

		String source= cuRewrite.getSource(node);
		Map<String, SourceLocation> results= new LinkedHashMap<>();
		if (comments.isEmpty()) {
			putResult(source, start, end, results);
		} else {
			int nextStart= start;
			for (Comment comment : comments) {
				if (nextStart < comment.getStartPosition()) {
					putResult(source, nextStart, comment.getStartPosition(), results);
				}
				nextStart= SourceLocation.getEndPosition(comment);
			}
		}

		return results;
	}

	private void putResult(final String source, final int start, final int end, final Map<String, SourceLocation> results) {
		SourceLocation sourceLoc= SourceLocation.fromPositions(start, end);
		String s= sourceLoc.substring(source);
		results.put(s, sourceLoc);
	}

	private List<Comment> filterCommentsInRange(final int start, final int end, final ASTNode root) {
		if (root instanceof CompilationUnit) {
			return ASTComments.filterCommentsInRange(start, end, (CompilationUnit) root);
		}

		return Collections.emptyList();
	}

	@Override
	public boolean visit(final TryStatement node) {
		@SuppressWarnings("unchecked")
		List<VariableDeclarationExpression> resources= node.resources();
		if (resources.isEmpty()) {
			return true;
		}
		VariableDeclarationExpression lastResource= resources.get(resources.size() - 1);
		Block body= node.getBody();
		return maybeRemoveSuperfluousSemiColons(node, SourceLocation.getEndPosition(lastResource), body.getStartPosition());
	}
}
