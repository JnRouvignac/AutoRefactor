/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Luis Cruz - Android Refactoring
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
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

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.getAncestor;
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import org.autorefactor.preferences.Preferences;
import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.FinderVisitor;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TypeDeclaration;

/** See {@link #getDescription()} method. */
public class AndroidWakeLockRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Android WakeLock";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Android - Failing to release a wakelock properly can keep the Android device "
            + "in a high power mode, which reduces battery life. "
            + "There are several causes for this, such as "
            + "releasing the wake lock in onDestroy() instead of in onPause(), "
            + "failing to call release() in all possible code paths after an acquire(), "
            + "and so on.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the performance.";
    }

    @Override
    public boolean isEnabled(Preferences preferences) {
        // FIXME enable only when android libraries are detected
        return super.isEnabled(preferences);
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (isMethod(node, "android.os.PowerManager.WakeLock", "release")) {
            // check whether it is being called in onDestroy()
            MethodDeclaration enclosingMethod = getAncestor(node, MethodDeclaration.class);
            if (isMethod(enclosingMethod, "android.app.Activity", "onDestroy")) {
                final Refactorings r = ctx.getRefactorings();
                TypeDeclaration typeDeclaration = getAncestor(enclosingMethod, TypeDeclaration.class);
                MethodDeclaration onPauseMethod = findMethod(typeDeclaration, "onPause");
                if (onPauseMethod != null && node.getParent().getNodeType() == ASTNode.EXPRESSION_STATEMENT) {
                    r.remove(node.getParent());
                    r.insertLast(
                            onPauseMethod.getBody(),
                            Block.STATEMENTS_PROPERTY,
                            createWakelockReleaseStmt(node));
                } else {
                    // Add the missing onPause() method to the class.
                    r.insertAfter(createOnPauseMethodDeclaration(), enclosingMethod);
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (isMethod(node, "android.os.PowerManager.WakeLock", "acquire")) {
            final Refactorings r = ctx.getRefactorings();
            TypeDeclaration typeDeclaration = getAncestor(node, TypeDeclaration.class);
            ReleasePresenceChecker releasePresenceChecker = new ReleasePresenceChecker();
            if (!releasePresenceChecker.findOrDefault(typeDeclaration, false)) {
                MethodDeclaration onPauseMethod = findMethod(typeDeclaration, "onPause");
                if (onPauseMethod != null && node.getParent().getNodeType() == ASTNode.EXPRESSION_STATEMENT) {
                    r.insertLast(
                            onPauseMethod.getBody(),
                            Block.STATEMENTS_PROPERTY,
                            createWakelockReleaseStmt(node));
                } else {
                    r.insertLast(
                            typeDeclaration,
                            typeDeclaration.getBodyDeclarationsProperty(),
                            createOnPauseMethodDeclaration());
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private Statement createWakelockReleaseStmt(MethodInvocation methodInvocation) {
        final ASTBuilder b = ctx.getASTBuilder();
        return  b.if0(
                b.not(b.invoke(b.copyExpression(methodInvocation), "isHeld")),
                b.block(b.toStmt(b.invoke(b.copyExpression(methodInvocation), "release"))));
    }

    private MethodDeclaration createOnPauseMethodDeclaration() {
        final ASTBuilder b = ctx.getASTBuilder();
        return b.method(
                b.extendedModifiers(b.annotation("Override"), b.protected0()),
                "onPause",
                b.parameters(),
                b.block(
                   b.toStmt(b.superInvoke("onPause"))));
    }

    private MethodDeclaration findMethod(TypeDeclaration typeDeclaration, String methodToFind) {
        if (typeDeclaration != null) {
            for (MethodDeclaration method : typeDeclaration.getMethods()) {
                IMethodBinding methodBinding = method.resolveBinding();
                if (methodBinding != null
                        && methodToFind.equals(methodBinding.getName())
                        && method.parameters().isEmpty()) {
                    return method;
                }
            }
        }
        return null;
    }

    private static class ReleasePresenceChecker extends FinderVisitor<Boolean> {
        @Override
        public boolean visit(MethodInvocation node) {
            if (isMethod(node, "android.os.PowerManager.WakeLock", "release")) {
                setResult(true);
                return DO_NOT_VISIT_SUBTREE;
            }
            return VISIT_SUBTREE;
        }
    }
}
