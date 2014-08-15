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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.SourceLocation;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.NodeFinder;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TextElement;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Refactor comments:
 * <ul>
 * <li>Remove empty comments</li>
 * <li>Transform comments into javadocs</li>
 * <li>Transform javadocs into block comments</li>
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
    private static final Pattern JAVADOC_ONLY_INHERITDOC =
            Pattern.compile("/\\*\\*\\s*(\\*\\s*)*\\{@inheritDoc\\}\\s*(\\*\\s*)*\\*/");
    private static final Pattern ECLIPSE_GENERATED_TODOS = Pattern.compile("//\\s*"
            + "(:?"
            +   "(?:TODO Auto-generated (?:(?:(?:method|constructor) stub)|(?:catch block)))"
            + "|"
            +   "(?:TODO: handle exception)"
            + ")"
            + "\\s*");
    private static final Pattern JAVADOC_WITHOUT_FINAL_DOT =
            Pattern.compile("(.*?)((?:\\s*(?:\\r|\\n|\\r\\n)*\\s*)*\\*/)", Pattern.DOTALL);
    private static final Pattern JAVADOC_FIRST_LETTER_LOWERCASE =
            Pattern.compile("(/\\*\\*(?:\\s*\\*(?:\\r|\\n|\\r\\n|\\s)))*(\\w)(.*)", Pattern.DOTALL);

    private RefactoringContext ctx;
    private CompilationUnit astRoot;
    private List<Pair<SourceLocation, Comment>> comments = new ArrayList<Pair<SourceLocation, Comment>>();

    /** Class constructor. */
    public CommentsRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(BlockComment node) {
        final String comment = getComment(node);
        if (EMPTY_BLOCK_COMMENT.matcher(comment).matches()) {
            this.ctx.getRefactorings().remove(node);
            return DO_NOT_VISIT_SUBTREE;
        }
        final ASTNode nextNode = getNextNode(node);
        if (acceptJavadoc(nextNode) && !betterCommentExist(node, nextNode)) {
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
        if (node instanceof Javadoc) {
            return finder.getCoveringNode();
        }
        return finder.getCoveredNode();
    }

    private ASTNode getCoveringNode(Comment node) {
        final int start = node.getStartPosition();
        final int length = node.getLength();
        final ASTNode coveringNode = getCoveringNode(start, length);
        if (coveringNode != node) {
            return coveringNode;
        }
        return getCoveringNode(start, length + 1);
    }

    private ASTNode getCoveringNode(int start, int length) {
        final ASTNode root = this.astRoot;
        final NodeFinder finder = new NodeFinder(root, start, length);
        return finder.getCoveringNode();
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Javadoc node) {
        final String comment = getComment(node);
        if (EMPTY_JAVADOC.matcher(comment).matches()) {
            this.ctx.getRefactorings().remove(node);
            return DO_NOT_VISIT_SUBTREE;
        } else if (allTagsEmpty(tags(node))) {
            this.ctx.getRefactorings().remove(node);
            return DO_NOT_VISIT_SUBTREE;
        } else if (JAVADOC_ONLY_INHERITDOC.matcher(comment).matches()) {
            // Put on one line only to augment vertical density of code
            int startLine = this.astRoot.getLineNumber(node.getStartPosition());
            int endLine = this.astRoot.getLineNumber(node.getStartPosition() + node.getLength());
            if (startLine != endLine) {
                this.ctx.getRefactorings().replace(node, "/** {@inheritDoc} */");
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (!acceptJavadoc(getNextNode(node))) {
            this.ctx.getRefactorings().replace(node, comment.replace("/**", "/*"));
            return DO_NOT_VISIT_SUBTREE;
        } else if (!comment.contains(".")) {
            Matcher matcher = JAVADOC_WITHOUT_FINAL_DOT.matcher(comment);
            if (matcher.matches()) {
                String newComment = matcher.group(1) + "." + matcher.group(2);
                this.ctx.getRefactorings().replace(node, newComment);
                return DO_NOT_VISIT_SUBTREE;
            }
        } else {
            Matcher m = JAVADOC_FIRST_LETTER_LOWERCASE.matcher(comment);
            if (m.matches() && Character.isLowerCase(m.group(2).charAt(0))) {
                String newComment = m.group(1) + m.group(2).toUpperCase() + m.group(3);
                if (!newComment.equals(comment)) {
                    this.ctx.getRefactorings().replace(node, newComment);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
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
//            } else if (TagElement.TAG_AUTHOR.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_CODE.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_DEPRECATED.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_DOCROOT.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_EXCEPTION.equals(tag.getTagName())) {
        } else if (TagElement.TAG_INHERITDOC.equals(tag.getTagName())) {
            return true;
//            } else if (TagElement.TAG_LINK.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_LINKPLAIN.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_LITERAL.equals(tag.getTagName())) {
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
//            } else if (TagElement.TAG_SEE.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_SERIAL.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_SERIALDATA.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_SERIALFIELD.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_SINCE.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_THROWS.equals(tag.getTagName())) {
            // TODO JNR a @throws tag repeating the checked exceptions of the method is useless
//            } else if (TagElement.TAG_VALUE.equals(tag.getTagName())) {
//            } else if (TagElement.TAG_VERSION.equals(tag.getTagName())) {
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

    /** {@inheritDoc} */
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
            if (acceptJavadoc(nextNode) && !betterCommentExist(node, nextNode)) {
                this.ctx.getRefactorings().toJavadoc(node, nextNode);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean betterCommentExist(Comment comment,
            ASTNode nodeWhereToAddJavadoc) {
        if (hasJavadoc(nodeWhereToAddJavadoc)) {
            return true;
        }

        final SourceLocation nodeLoc = new SourceLocation(nodeWhereToAddJavadoc);
        SourceLocation bestLoc = new SourceLocation(comment);
        Comment bestComment = comment;
        for (Iterator<Pair<SourceLocation, Comment>> iter = this.comments.iterator(); iter.hasNext();) {
            final Pair<SourceLocation, Comment> pair = iter.next();
            final SourceLocation newLoc = pair.getFirst();
            final Comment newComment = pair.getSecond();
            if (newLoc.compareTo(bestLoc) < 0) {
                // since comments are visited in ascending order,
                // we can forget this comment.
                iter.remove();
                continue;
            }
            if (nodeLoc.compareTo(newLoc) < 0) {
                break;
            }
            if (bestLoc.compareTo(newLoc) < 0) {
                if (!(newComment instanceof LineComment)) {
                    // new comment is a BlockComment or a Javadoc
                    bestLoc = newLoc;
                    bestComment = newComment;
                    continue;
                } else if (!(bestComment instanceof LineComment)) {
                    // new comment is a line comment and best comment is not
                    bestLoc = newLoc;
                    bestComment = newComment;
                    continue;
                }
            }
        }
        return bestComment != null && bestComment != comment;
    }

    private boolean hasJavadoc(ASTNode node) {
        if (node instanceof BodyDeclaration) {
            return ((BodyDeclaration) node).getJavadoc() != null;
        } else if (node instanceof PackageDeclaration) {
            return ((PackageDeclaration) node).getJavadoc() != null;
        }
        return false;
    }

    private boolean acceptJavadoc(final ASTNode node) {
        // PackageDeclaration node accept javadoc in package-info.java files,
        // but they are useless everywhere else.
        return node instanceof BodyDeclaration
            || (node instanceof PackageDeclaration
                && "package-info.java".equals(getFileName(node)));
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(CompilationUnit node) {
        this.astRoot = node;
        for (Comment comment : getCommentList(astRoot)) {
            comments.add(Pair.of(new SourceLocation(comment), comment));
        }

        for (Comment comment : getCommentList(astRoot)) {
            if (comment.isBlockComment()) {
                final BlockComment bc = (BlockComment) comment;
                bc.accept(this);
            } else if (comment.isLineComment()) {
                final LineComment lc = (LineComment) comment;
                lc.accept(this);
            } else if (comment.isDocComment()) {
                final Javadoc jc = (Javadoc) comment;
                jc.accept(this);
            } else {
                throw new NotImplementedException(comment);
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
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
