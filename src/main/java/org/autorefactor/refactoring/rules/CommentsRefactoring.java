/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;
import java.util.regex.Pattern;

import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeMemberDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EnumConstantDeclaration;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.NodeFinder;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TextElement;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Refactor comments:
 * <ul>
 * <li>Remove empty comments</li>
 * <li>Transform comments into javadocs</li>
 * <li>TODO Transform javadocs into comments</li>
 * <li>Remove IDE generated TODOs</li>
 * <li>TODO Remove commented out code</li>
 * <li>TODO Fix malformed/incomplete javadocs</li>
 * <li>TODO Fix typo in comments</li>
 * </ul>
 */
public class CommentsRefactoring extends ASTVisitor implements IJavaRefactoring {

	private static final Pattern EMPTY_LINE_COMMENT = Pattern.compile("//\\s*");
	private static final Pattern EMPTY_BLOCK_COMMENT = Pattern.compile("/\\*\\s*(\\*\\s*)*\\*/");
	private static final Pattern EMPTY_JAVADOC = Pattern.compile("/\\*\\*\\s*(\\*\\s*)*\\*/");
	private static final Pattern ECLIPSE_GENERATED_TODOS = Pattern.compile("//\\s*"
			+ "(:?"
			+   "(?:TODO Auto-generated (?:(?:(?:method|constructor) stub)|(?:catch block)))"
			+ "|"
			+   "(?:TODO: handle exception)"
			+ ")"
			+ "\\s*");

	private RefactoringContext ctx;
	private CompilationUnit astRoot;

	public CommentsRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
	}

	@Override
	public boolean visit(BlockComment node) {
		final String comment = getComment(node);
		if (EMPTY_BLOCK_COMMENT.matcher(comment).matches()) {
			this.ctx.getRefactorings().remove(node);
			return DO_NOT_VISIT_SUBTREE;
		} else if (acceptJavadoc(getNextNode(node))) {
			this.ctx.getRefactorings().toJavadoc(node);
			return DO_NOT_VISIT_SUBTREE;
		}
		return VISIT_SUBTREE;
	}

	private ASTNode getNextNode(Comment node) {
		final int nodeEndPosition = node.getStartPosition() + node.getLength();
		final ASTNode root = getCoveringNode(node);
		final int parentNodeEndPosition = root.getStartPosition() + root.getLength();
		final NodeFinder finder = new NodeFinder(root,
				nodeEndPosition, parentNodeEndPosition - nodeEndPosition);
		return finder.getCoveredNode();
	}

	private ASTNode getCoveringNode(Comment node) {
		final ASTNode root = this.astRoot;
		final NodeFinder finder = new NodeFinder(root, node.getStartPosition(),
				node.getLength());
		return finder.getCoveringNode();
	}

	@Override
	public boolean visit(Javadoc node) {
		final String comment = getComment(node);
		if (EMPTY_JAVADOC.matcher(comment).matches()) {
			this.ctx.getRefactorings().remove(node);
			return DO_NOT_VISIT_SUBTREE;
		} else if (allTagsEmpty(node.tags())) {
			this.ctx.getRefactorings().remove(node);
			return DO_NOT_VISIT_SUBTREE;
		} else if (!acceptJavadoc(getNextNode(node))) {
			// TODO JNR convert to block comment
		}
		return VISIT_SUBTREE;
	}

	private boolean allTagsEmpty(List<TagElement> tags) {
		return !anyTagNotEmpty(tags, false);
	}

	/**
	 * A tag is considered empty when it does not provide any useful information
	 * beyond what is already in the code.
	 *
	 * @param tags the tags to look for emptiness
	 * @param throwIfUnknown only useful for debugging. For now, default is to not remove tag or throw when unknown
	 * @return true if any tag is not empty, false otherwise
	 */
	private boolean anyTagNotEmpty(List<TagElement> tags, boolean throwIfUnknown) {
		if (tags.isEmpty()) {
			return false;
		}
		for (TagElement tag : tags) {
			if (isNotEmpty(tag, throwIfUnknown)) {
				return true;
			}
		}
		return false;
	}

	private boolean isNotEmpty(TagElement tag, boolean throwIfUnknown) {
		if (tag.getTagName() == null) {
			if (anyTextElementNotEmpty(tag.fragments(), throwIfUnknown)) {
				return true;
			}
//			} else if (TagElement.TAG_AUTHOR.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_CODE.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_DEPRECATED.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_DOCROOT.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_EXCEPTION.equals(tag.getTagName())) {
		} else if (TagElement.TAG_INHERITDOC.equals(tag.getTagName())) {
			return true;
//			} else if (TagElement.TAG_LINK.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_LINKPLAIN.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_LITERAL.equals(tag.getTagName())) {
		} else if (TagElement.TAG_PARAM.equals(tag.getTagName())) {
			if (anyTextElementNotEmpty(tag.fragments(), throwIfUnknown)) {
				// TODO JNR a @param tag repeating the parameters of the method is useless
				return true;
			}
		} else if (TagElement.TAG_RETURN.equals(tag.getTagName())) {
			if (anyTextElementNotEmpty(tag.fragments(), throwIfUnknown)) {
				// TODO JNR a return tag repeating the return type of the method is useless
				return true;
			}
//			} else if (TagElement.TAG_SEE.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_SERIAL.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_SERIALDATA.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_SERIALFIELD.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_SINCE.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_THROWS.equals(tag.getTagName())) {
			// TODO JNR a @throws tag repeating the checked exceptions of the method is useless
//			} else if (TagElement.TAG_VALUE.equals(tag.getTagName())) {
//			} else if (TagElement.TAG_VERSION.equals(tag.getTagName())) {
		} else if (throwIfUnknown) {
			throw new NotImplementedException("for tagName " + tag.getTagName());
		}
		return false;
	}

	private boolean anyTextElementNotEmpty(List<?> fragments, boolean throwIfUnknown) {
		for (/* IDocElement */ Object fragment : fragments) {
			if (fragment instanceof TextElement) {
				String text = ((TextElement) fragment).getText();
				if (text != null && text.length() > 0) {
					return true;
				}
			} else if (fragment instanceof TagElement) {
				if (isNotEmpty((TagElement) fragment, throwIfUnknown)) {
					return true;
				}
			} else if (throwIfUnknown) {
				// It could be one of the following:
				// org.eclipse.jdt.core.dom.MemberRef
				// org.eclipse.jdt.core.dom.MethodRef
				// org.eclipse.jdt.core.dom.Name
				throw new NotImplementedException(fragment);
			}
		}
		return false;
	}

	@Override
	public boolean visit(LineComment node) {
		final String comment = getComment(node);
		if (EMPTY_LINE_COMMENT.matcher(comment).matches()) {
			this.ctx.getRefactorings().remove(node);
			return DO_NOT_VISIT_SUBTREE;
		} else if (ECLIPSE_GENERATED_TODOS.matcher(comment).matches()) {
			this.ctx.getRefactorings().remove(node);
			return DO_NOT_VISIT_SUBTREE;
		} else {
			final ASTNode nextNode = getNextNode(node);
			if (acceptJavadoc(nextNode)) {
				this.ctx.getRefactorings().toJavadoc(node, nextNode);
				return DO_NOT_VISIT_SUBTREE;
			}
		}
		return VISIT_SUBTREE;
	}

	private boolean acceptJavadoc(final ASTNode node) {
		if (node == null) {
			return false;
		}
		return node instanceof AbstractTypeDeclaration
				// covers annotation, enums, classes and interfaces
				|| node instanceof AnonymousClassDeclaration
				|| node instanceof MethodDeclaration
				|| node instanceof FieldDeclaration
				|| node instanceof AnnotationTypeMemberDeclaration
				|| node instanceof EnumConstantDeclaration;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		this.astRoot = astRoot;
		for (Comment comment : (List<Comment>) astRoot.getCommentList()) {
			if (comment.isBlockComment()) {
				final BlockComment bc = (BlockComment) comment;
				bc.accept(this);
			} else if (comment.isLineComment()) {
				final LineComment lc = (LineComment) comment;
				lc.accept(this);
			} else if (comment.isDocComment()) {
				final Javadoc jc = (Javadoc) comment;
				jc.accept(this);
			}
		}
		return this.ctx.getRefactorings();
	}

	private String getComment(Comment node) {
		try {
			final String source = this.ctx.getCompilationUnit().getSource();
			final int start = node.getStartPosition();
			return source.substring(start, start + node.getLength());
		} catch (JavaModelException e) {
			throw new RuntimeException(e);
		}
	}

}
