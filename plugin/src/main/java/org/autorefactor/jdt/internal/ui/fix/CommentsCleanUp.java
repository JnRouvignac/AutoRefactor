/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Sameer Misger - Make SonarQube more happy
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
import java.util.Iterator;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.SourceLocation;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.MarkerAnnotation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NodeFinder;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TextElement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.text.edits.TextEditGroup;

/**
 * See {@link #getDescription()} method.
 *
 * <ul>
 * <li>TODO Remove commented out code</li>
 * <li>TODO Fix malformed/incomplete javadocs</li>
 * <li>TODO Fix typo in comments</li>
 * </ul>
 */
public class CommentsCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CommentsCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CommentsCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CommentsCleanUp_reason;
	}

	private static final Pattern EMPTY_LINE_COMMENT= Pattern.compile("//\\s*"); //$NON-NLS-1$
	private static final Pattern EMPTY_BLOCK_COMMENT= Pattern.compile("/\\*\\s*(\\*\\s*)*\\*/"); //$NON-NLS-1$
	private static final Pattern EMPTY_JAVADOC= Pattern.compile("/\\*\\*\\s*(\\*\\s*)*\\*/"); //$NON-NLS-1$
	private static final Pattern EMPTY_LINE_AT_START_OF_BLOCK_COMMENT= Pattern.compile("(/\\*)(?:\\s*\\*)+(\\s*\\*)"); //$NON-NLS-1$
	private static final Pattern EMPTY_LINE_AT_START_OF_JAVADOC= Pattern.compile("(/\\*\\*)(?:\\s*\\*)+(\\s*\\*)"); //$NON-NLS-1$
	private static final Pattern EMPTY_LINE_AT_END_OF_BLOCK_COMMENT= Pattern.compile("(?:\\*\\s*)*\\*\\s*(\\*/)"); //$NON-NLS-1$
	private static final Pattern JAVADOC_ONLY_INHERITDOC= Pattern
			.compile("/\\*\\*\\s*(\\*\\s*)*\\{@inheritDoc\\}\\s*(\\*\\s*)*\\*/"); //$NON-NLS-1$
	private static final Pattern ECLIPSE_GENERATED_TODOS= Pattern
			.compile("//\\s*" + "(:?" + "(?:TODO Auto-generated (?:(?:(?:method|constructor) stub)|(?:catch block)))" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					+ "|" + "(?:TODO: handle exception)" + ")" + "\\s*"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	private static final Pattern ECLIPSE_GENERATED_NON_JAVADOC= Pattern
			.compile("/\\*\\s*\\(non-Javadoc\\)\\s*\\*\\s*@see\\s*"); //$NON-NLS-1$
	private static final Pattern ECLIPSE_IGNORE_NON_EXTERNALIZED_STRINGS= Pattern
			.compile("//\\s*\\$NON-NLS-\\d\\$\\s*"); //$NON-NLS-1$
	/**
	 * Ignore special instructions to locally enable/disable tools working on java
	 * code:
	 * <ul>
	 * <li>checkstyle</li>
	 * <li>JDT</li>
	 * </ul>
	 */
	private static final Pattern TOOLS_CONTROL_INSTRUCTIONS= Pattern.compile("//\\s*@\\w+:\\w+"); //$NON-NLS-1$
	private static final Pattern JAVADOC_HAS_PUNCTUATION= Pattern.compile("\\.|\\?|!|:"); //$NON-NLS-1$
	private static final Pattern JAVADOC_WITHOUT_PUNCTUATION=
			// @formatter:off
			Pattern.compile("(.*?)" + "(" + "\\s*" + "(?:" + "\\*" + "/" + ")?" + ")", Pattern.DOTALL); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
	// @formatter:on
	private static final Pattern FIRST_JAVADOC_TAG=
			// @formatter:off
			Pattern.compile("(" + "/\\*\\*" + ")?" + "\\s*" + "(?:\\*\\s*)?" + "@\\w+", Pattern.MULTILINE); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
	// @formatter:on
	private static final Pattern JAVADOC_FIRST_LETTER_LOWERCASE= Pattern
			.compile("(/\\*\\*\\s*(?:(?:\\rewrite|\\n|\\rewrite\\n|\\s)\\s*\\*)*\\s*)(\\w)(.*)", Pattern.DOTALL); //$NON-NLS-1$

	private CompilationUnit astRoot;
	private final List<Pair<SourceLocation, Comment>> comments= new ArrayList<>();

	@Override
	public boolean visit(final BlockComment node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CommentsCleanUp_description);

		String comment= getComment(node);

		if (EMPTY_BLOCK_COMMENT.matcher(comment).matches()) {
			rewrite.remove(node, group);
			return false;
		}

		ASTNode nextNode= getNextNode(node);

		if (acceptJavadoc(nextNode) && !betterCommentExist(node, nextNode)) {
			if (ECLIPSE_GENERATED_NON_JAVADOC.matcher(comment).find()) {
				rewrite.remove(node, group);
			} else {
				rewrite.toJavadoc(node);
			}

			return false;
		}

		Matcher emptyLineAtStartMatcher= EMPTY_LINE_AT_START_OF_BLOCK_COMMENT.matcher(comment);

		if (emptyLineAtStartMatcher.find()) {
			replaceEmptyLineAtStartOfComment(node, emptyLineAtStartMatcher);
			return false;
		}

		Matcher emptyLineAtEndMatcher= EMPTY_LINE_AT_END_OF_BLOCK_COMMENT.matcher(comment);

		if (emptyLineAtEndMatcher.find()) {
			replaceEmptyLineAtEndOfComment(node, emptyLineAtEndMatcher);
			return false;
		}

		String replacement= getReplacement(comment, false);

		if (replacement != null && !replacement.equals(comment)) {
			rewrite.replace(node, replacement);
			return false;
		}

		return true;
	}

	private String getReplacement(final String comment, final boolean isJavadoc) {
		int commentLineLength= cuRewrite.getJavaProjectOptions().getCommentLineLength();
		String commentNoStartNorEnd= comment.substring(0, comment.length() - 2).substring(isJavadoc ? 3 : 2);
		String commentWithSpaces= commentNoStartNorEnd.replaceAll("\\s*(\\rewrite\\n|\\rewrite|\\n)\\s*\\*", " "); //$NON-NLS-1$ //$NON-NLS-2$
		String commentContent= commentWithSpaces.replaceAll("\\s+", " ").trim(); //$NON-NLS-1$ //$NON-NLS-2$
		if (commentContent.length() + (isJavadoc ? 7 : 6) < commentLineLength) {
			return (isJavadoc ? "/** " : "/* ") + commentContent + " */"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}

		return null;
	}

	private ASTNode getNextNode(final Comment node) {
		int nodeEndPosition= node.getStartPosition() + node.getLength();
		ASTNode coveringNode= getCoveringNode(node);
		int parentNodeEndPosition= coveringNode.getStartPosition() + coveringNode.getLength();
		NodeFinder finder= new NodeFinder(coveringNode, nodeEndPosition, parentNodeEndPosition - nodeEndPosition);
		if (node instanceof Javadoc) {
			return finder.getCoveringNode();
		}

		return finder.getCoveredNode();
	}

	private ASTNode getCoveringNode(final Comment node) {
		int start= node.getStartPosition();
		int length= node.getLength();
		ASTNode coveringNode= getCoveringNode(start, length);
		if (coveringNode != node) {
			return coveringNode;
		}

		return getCoveringNode(start, length + 1);
	}

	private ASTNode getCoveringNode(final int start, final int length) {
		NodeFinder finder= new NodeFinder(this.astRoot, start, length);
		return finder.getCoveringNode();
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean visit(final Javadoc node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CommentsCleanUp_description);

		String comment= getComment(node);
		boolean isWellFormattedInheritDoc= "/** {@inheritDoc} */".equals(comment); //$NON-NLS-1$
		Matcher emptyLineAtStartMatcher= EMPTY_LINE_AT_START_OF_JAVADOC.matcher(comment);
		Matcher emptyLineAtEndMatcher= EMPTY_LINE_AT_END_OF_BLOCK_COMMENT.matcher(comment);
		if (EMPTY_JAVADOC.matcher(comment).matches()) {
			rewrite.remove(node, group);
			return false;
		}

		if (emptyLineAtStartMatcher.find()) {
			replaceEmptyLineAtStartOfComment(node, emptyLineAtStartMatcher);
			return false;
		}

		if (emptyLineAtEndMatcher.find()) {
			replaceEmptyLineAtEndOfComment(node, emptyLineAtEndMatcher);
			return false;
		}

		if (allTagsEmpty(node.tags())) {
			rewrite.remove(node, group);
			return false;
		}

		if (!acceptJavadoc(getNextNode(node)) && node.getStartPosition() != 0) {
			rewrite.replace(node, comment.replace("/**", "/*")); //$NON-NLS-1$ //$NON-NLS-2$
			return false;
		}

		if (JAVADOC_ONLY_INHERITDOC.matcher(comment).matches()) {
			ASTNode nextNode= getNextNode(node);
			if (hasOverrideAnnotation(nextNode)) {
				// {@inheritDoc} tag is redundant with @Override annotation
				rewrite.remove(node, group);
				return false;
			}

			if (!isWellFormattedInheritDoc) {
				// Put on one line only, to augment vertical density of code
				int startLine= this.astRoot.getLineNumber(node.getStartPosition());
				int endLine= this.astRoot.getLineNumber(node.getStartPosition() + node.getLength());
				if (startLine != endLine) {
					rewrite.replace(node, "/** {@inheritDoc} */"); //$NON-NLS-1$
					return false;
				}
			}
		} else if (!isWellFormattedInheritDoc && !JAVADOC_HAS_PUNCTUATION.matcher(comment).find()) {
			String newComment= addPeriodAtEndOfFirstLine(node, comment);
			if (newComment != null) {
				rewrite.replace(node, newComment);
				return false;
			}
		} else {
			Matcher m= JAVADOC_FIRST_LETTER_LOWERCASE.matcher(comment);
			if (m.matches() && Character.isLowerCase(m.group(2).charAt(0))) {
				String newComment= m.group(1) + m.group(2).toUpperCase() + m.group(3);
				if (!newComment.equals(comment)) {
					rewrite.replace(node, newComment);
					return false;
				}
			}
		}

		if (hasNoTags(node)) {
			String replacement= getReplacement(comment, true);
			if (replacement != null && !replacement.equals(comment)) {
				rewrite.replace(node, replacement);
				return false;
			}
		}

		return true;
	}

	@SuppressWarnings("unchecked")
	private boolean hasOverrideAnnotation(final ASTNode node) {
		if (node instanceof BodyDeclaration) {
			for (IExtendedModifier modifier : (List<IExtendedModifier>) ((BodyDeclaration) node).modifiers()) {
				return ASTNodes.hasType(getTypeName(modifier), Override.class.getCanonicalName());
			}
		}

		return false;
	}

	private Name getTypeName(final IExtendedModifier extendedModifier) {
		if (extendedModifier instanceof MarkerAnnotation) {
			return ((MarkerAnnotation) extendedModifier).getTypeName();
		}
		if (extendedModifier instanceof NormalAnnotation) {
			return ((NormalAnnotation) extendedModifier).getTypeName();
		}

		return null;
	}

	@SuppressWarnings("unchecked")
	private boolean hasNoTags(final Javadoc node) {
		for (TagElement tag : (List<TagElement>) node.tags()) {
			if (tag.getTagName() != null) {
				return false;
			}
		}

		return true;
	}

	private void replaceEmptyLineAtStartOfComment(final Comment node, final Matcher matcher) {
		String replacement= matcher.replaceFirst(matcher.group(1) + matcher.group(2));
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CommentsCleanUp_description);
		cuRewrite.getASTRewrite().replace(node, replacement);
	}

	private void replaceEmptyLineAtEndOfComment(final Comment node, final Matcher matcher) {
		String replacement= matcher.replaceFirst(matcher.group(1));
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CommentsCleanUp_description);
		cuRewrite.getASTRewrite().replace(node, replacement);
	}

	private String addPeriodAtEndOfFirstLine(final Javadoc node, final String comment) {
		String beforeFirstTag= comment;
		String afterFirstTag= ""; //$NON-NLS-1$
		Matcher m= FIRST_JAVADOC_TAG.matcher(comment);
		if (m.find()) {
			if (m.start() == 0) {
				return null;
			}
			beforeFirstTag= comment.substring(0, m.start());
			afterFirstTag= comment.substring(m.start());
		}
		Matcher matcher= JAVADOC_WITHOUT_PUNCTUATION.matcher(beforeFirstTag);
		if (matcher.matches()) {
			@SuppressWarnings("unchecked")
			List<TagElement> tagElements= node.tags();
			if (tagElements.size() >= 2) {
				TagElement firstLine= tagElements.get(0);
				int relativeStart= firstLine.getStartPosition() - node.getStartPosition();
				int endOfFirstLine= relativeStart + firstLine.getLength();
				return comment.substring(0, endOfFirstLine) + "." + comment.substring(endOfFirstLine); //$NON-NLS-1$
				// TODO JNR do the replace here, not outside this method
			}

			return matcher.group(1) + "." + matcher.group(2) + afterFirstTag; //$NON-NLS-1$
		}

		return null;
	}

	private boolean allTagsEmpty(final List<TagElement> tags) {
		return !anyTagNotEmpty(tags, false);
	}

	/**
	 * A tag is considered empty when it does not provide any useful information
	 * beyond what is already in the code.
	 *
	 * @param tags           the tags to look for emptiness
	 * @param throwIfUnknown only useful for debugging. For now, default is to not
	 *                       remove tag or throw when unknown
	 * @return true if any tag is not empty, false otherwise
	 */
	private boolean anyTagNotEmpty(final List<TagElement> tags, final boolean throwIfUnknown) {
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

	private boolean isNotEmpty(final TagElement tag, final boolean throwIfUnknown) {
		String tagName= tag.getTagName();
		if (tagName == null || TagElement.TAG_AUTHOR.equals(tagName)
		// || TAG_CODE.equals(tagName)
				|| TagElement.TAG_DEPRECATED.equals(tagName)
				// || TAG_DOCROOT.equals(tagName)
				// || TAG_LINK.equals(tagName)
				// || TAG_LINKPLAIN.equals(tagName)
				// || TAG_LITERAL.equals(tagName)
				|| TagElement.TAG_RETURN.equals(tagName) || TagElement.TAG_SEE.equals(tagName) || TagElement.TAG_SERIAL.equals(tagName)
				|| TagElement.TAG_SERIALDATA.equals(tagName) || TagElement.TAG_SERIALFIELD.equals(tagName) || TagElement.TAG_SINCE.equals(tagName)
				// || TAG_VALUE.equals(tagName)
				|| TagElement.TAG_VERSION.equals(tagName)) {
			// TODO JNR a return tag repeating the return type of the method is useless
			return anyTextElementNotEmpty(tag.fragments(), throwIfUnknown);
		}
		if (TagElement.TAG_EXCEPTION.equals(tagName) || TagElement.TAG_PARAM.equals(tagName) || TagElement.TAG_THROWS.equals(tagName)) {
			// TODO JNR a @throws tag repeating the checked exceptions of the method is
			// useless
			return !isTagEmptyOrWithSimpleNameOnly(tag);
		}
		if (!TagElement.TAG_INHERITDOC.equals(tagName) && throwIfUnknown) {
			throw new NotImplementedException(tag, "for tagName " + tagName); //$NON-NLS-1$
		}

		return true;
	}

	private boolean isTagEmptyOrWithSimpleNameOnly(final TagElement tag) {
		switch (tag.fragments().size()) {
		case 0:
			return true;
		case 1:
			return tag.fragments().get(0) instanceof SimpleName;
		default:
			return false;
		}
	}

	private boolean anyTextElementNotEmpty(final List<?> fragments, final boolean throwIfUnknown) {
		for (/* IDocElement */ Object fragment : fragments) {
			if (fragment instanceof TextElement) {
				String text= ((TextElement) fragment).getText();
				if (text != null && !text.isEmpty()) {
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
				ASTNode node= fragment instanceof ASTNode ? (ASTNode) fragment : null;
				throw new NotImplementedException(node, fragment);
			}
		}

		return false;
	}

	@Override
	public boolean visit(final LineComment node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CommentsCleanUp_description);

		String comment= getComment(node);

		if (EMPTY_LINE_COMMENT.matcher(comment).matches() || ECLIPSE_GENERATED_TODOS.matcher(comment).matches()) {
			rewrite.remove(node, group);
			return false;
		}

		if (!TOOLS_CONTROL_INSTRUCTIONS.matcher(comment).matches()
				&& !ECLIPSE_IGNORE_NON_EXTERNALIZED_STRINGS.matcher(comment).matches()) {
			ASTNode nextNode= getNextNode(node);
			ASTNode previousNode= getPreviousSibling(nextNode);

			if (previousNode != null && isSameLineNumber(node, previousNode)) {
				rewrite.toJavadoc(node, previousNode);
				return false;
			}

			if (acceptJavadoc(nextNode) && !betterCommentExist(node, nextNode)) {
				rewrite.toJavadoc(node, nextNode);
				return false;
			}
		}

		return true;
	}

	private boolean isSameLineNumber(final LineComment node, final ASTNode previousNode) {
		CompilationUnit cu= (CompilationUnit) previousNode.getRoot();
		int lineNb1= cu.getLineNumber(node.getStartPosition());
		int lineNb2= cu.getLineNumber(previousNode.getStartPosition());
		return lineNb1 == lineNb2;
	}

	private ASTNode getPreviousSibling(final ASTNode node) {
		if (node != null && node.getParent() instanceof TypeDeclaration) {
			boolean isPrevious= true;
			TypeDeclaration typeDecl= (TypeDeclaration) node.getParent();

			TreeMap<Integer, ASTNode> nodes= new TreeMap<>();
			addAll(nodes, typeDecl.getFields());
			addAll(nodes, typeDecl.getMethods());
			addAll(nodes, typeDecl.getTypes());

			if (isPrevious) {
				SortedMap<Integer, ASTNode> entries= nodes.headMap(node.getStartPosition());
				if (!entries.isEmpty()) {
					return entries.get(entries.lastKey());
				}
			} else {
				SortedMap<Integer, ASTNode> entries= nodes.tailMap(node.getStartPosition());
				if (!entries.isEmpty()) {
					return entries.get(entries.firstKey());
				}
			}
		}

		return null;
	}

	private <T extends ASTNode> void addAll(final TreeMap<Integer, ASTNode> nodeMap, final T[] nodes) {
		for (T node : nodes) {
			nodeMap.put(node.getStartPosition(), node);
		}
	}

	private boolean betterCommentExist(final Comment comment, final ASTNode nodeWhereToAddJavadoc) {
		if (hasJavadoc(nodeWhereToAddJavadoc)) {
			return true;
		}

		SourceLocation nodeLoc= new SourceLocation(nodeWhereToAddJavadoc);
		SourceLocation bestLoc= new SourceLocation(comment);
		Comment bestComment= comment;
		for (Iterator<Pair<SourceLocation, Comment>> iter= this.comments.iterator(); iter.hasNext();) {
			Pair<SourceLocation, Comment> pair= iter.next();
			SourceLocation newLoc= pair.getFirst();
			Comment newComment= pair.getSecond();
			if (newLoc.compareTo(bestLoc) < 0) {
				// Since comments are visited in ascending order,
				// we can forget this comment.
				iter.remove();
				continue;
			}
			if (nodeLoc.compareTo(newLoc) < 0) {
				break;
			}
			if (bestLoc.compareTo(newLoc) < 0
					&& (!(newComment instanceof LineComment) || !(bestComment instanceof LineComment))) {
				// New comment is a BlockComment or a Javadoc
				bestLoc= newLoc;
				bestComment= newComment;
				continue;
			}
		}

		return bestComment != null && bestComment != comment;
	}

	private boolean hasJavadoc(final ASTNode node) {
		if (node instanceof BodyDeclaration) {
			return ((BodyDeclaration) node).getJavadoc() != null;
		}

		return node instanceof PackageDeclaration && ((PackageDeclaration) node).getJavadoc() != null;
	}

	private boolean acceptJavadoc(final ASTNode node) {
		// PackageDeclaration node accept javadoc in package-info.java files,
		// but they are useless everywhere else.
		return node instanceof BodyDeclaration
				|| (node instanceof PackageDeclaration && "package-info.java".equals(ASTNodes.getFileName(node))); //$NON-NLS-1$
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean visit(final CompilationUnit node) {
		comments.clear();
		this.astRoot= node;

		for (Comment comment : (List<Comment>) astRoot.getCommentList()) {
			comments.add(Pair.of(new SourceLocation(comment), comment));
		}

		for (Comment comment : (List<Comment>) astRoot.getCommentList()) {
			if (comment.isBlockComment()) {
				BlockComment bc= (BlockComment) comment;
				bc.accept(this);
			} else if (comment.isLineComment()) {
				LineComment lc= (LineComment) comment;
				lc.accept(this);
			} else if (comment.isDocComment()) {
				Javadoc jc= (Javadoc) comment;
				jc.accept(this);
			} else {
				throw new NotImplementedException(comment);
			}
		}

		return true;
	}

	private String getComment(final Comment node) {
		String source= cuRewrite.getSource(node);
		int start= node.getStartPosition();
		return source.substring(start, start + node.getLength());
	}
}
