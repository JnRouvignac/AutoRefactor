/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class NIORatherThanIOCleanUp extends NewClassImportCleanUp {
	private static final String GET_METHOD= "get"; //$NON-NLS-1$
	private static final String TOPATH_METHOD= "toPath"; //$NON-NLS-1$
	private static final String TO_U_R_I_METHOD= "toURI"; //$NON-NLS-1$
	private static final String TOURI_METHOD= "toUri"; //$NON-NLS-1$

	private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
		@Override
		public boolean visit(final MethodInvocation visited) {
			return maybeRefactorMethodInvocation(visited, getClassesToUseWithImport(), getImportsToAdd());
		}
		@Override
		public boolean visit(final Block visited) {
			return maybeRefactorBlock(visited, getClassesToUseWithImport(), getImportsToAdd());
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.NIORatherThanIOCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.NIORatherThanIOCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.NIORatherThanIOCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 7;
	}

	@Override
	public RefactoringWithObjectsClass getRefactoringClassInstance() {
		return new RefactoringWithObjectsClass();
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(Path.class.getCanonicalName(), Paths.class.getCanonicalName()));
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		return maybeRefactorMethodInvocation(visited, getAlreadyImportedClasses(visited), new HashSet<String>());
	}

	private boolean maybeRefactorMethodInvocation(final MethodInvocation visited,
			final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		if (isFileCreation(visited.getExpression())
				&& isFileUse(visited.getExpression())) {
			refactorMethodInvocation(visited, classesToUseWithImport, importsToAdd);
			return false;
		}

		return true;
	}

	private void refactorMethodInvocation(final MethodInvocation visited, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.NIORatherThanIOCleanUp_description);

		String pathsName= addImport(Paths.class, classesToUseWithImport, importsToAdd);

		ClassInstanceCreation classInstanceCreation= (ClassInstanceCreation) visited.getExpression();
		Expression copyOfPathText= ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression((Expression) classInstanceCreation.arguments().get(0)));

		MethodInvocation getMethod= ast.newMethodInvocation();
		getMethod.setExpression(ASTNodeFactory.newName(ast, pathsName));
		getMethod.setName(ast.newSimpleName(GET_METHOD));
		getMethod.arguments().add(copyOfPathText);

		if (ASTNodes.usesGivenSignature(visited, File.class.getCanonicalName(), TOPATH_METHOD)) {
			ASTNodes.replaceButKeepComment(rewrite, visited, getMethod, group);
		} else {
			MethodInvocation methodInvocation= ast.newMethodInvocation();
			methodInvocation.setExpression(getMethod);
			methodInvocation.setName(ast.newSimpleName(TOURI_METHOD));
			MethodInvocation newMethodInvocation= methodInvocation;
			ASTNodes.replaceButKeepComment(rewrite, visited, newMethodInvocation, group);
		}
	}

	@Override
	public boolean visit(final Block visited) {
		return maybeRefactorBlock(visited, getAlreadyImportedClasses(visited), new HashSet<String>());
	}

	private boolean maybeRefactorBlock(final Block visited,
			final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		FileAndUsesVisitor fileAndUsesVisitor= new FileAndUsesVisitor(classesToUseWithImport, importsToAdd);
		fileAndUsesVisitor.visitNode(visited);
		return fileAndUsesVisitor.result;
	}

	private final class FileAndUsesVisitor extends BlockSubVisitor {
		private final Set<String> classesToUseWithImport;
		private final Set<String> importsToAdd;

		public FileAndUsesVisitor(final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
			this.classesToUseWithImport= classesToUseWithImport;
			this.importsToAdd= importsToAdd;
		}

		@Override
		public boolean visit(final VariableDeclarationStatement visited) {
			if (visited.fragments().size() != 1) {
				return true;
			}

			VariableDeclarationFragment fragment= (VariableDeclarationFragment) visited.fragments().get(0);
			return visitVariable(visited.getType(), fragment.resolveBinding(), fragment.getExtraDimensions(), fragment.getInitializer());
		}

		@Override
		public boolean visit(final VariableDeclarationExpression visited) {
			if (visited.fragments().size() != 1) {
				return true;
			}

			VariableDeclarationFragment fragment= (VariableDeclarationFragment) visited.fragments().get(0);
			return visitVariable(visited.getType(), fragment.resolveBinding(), fragment.getExtraDimensions(), fragment.getInitializer());
		}

		@Override
		public boolean visit(final SingleVariableDeclaration visited) {
			return visitVariable(visited.getType(), visited.resolveBinding(), visited.getExtraDimensions(), visited.getInitializer());
		}

		private boolean visitVariable(final Type type, final IVariableBinding variableBinding, final int extraDimensions, final Expression initializer) {
			if (result && ASTNodes.hasType(type.resolveBinding(), File.class.getCanonicalName())
					&& extraDimensions == 0
					&& initializer != null) {
				if (isFileCreation(initializer)) {
					VarDefinitionsUsesVisitor varOccurrencesVisitor= new VarDefinitionsUsesVisitor(variableBinding,
					startNode, true);

					List<SimpleName> reads= varOccurrencesVisitor.getReads();
					List<SimpleName> writes= varOccurrencesVisitor.getWrites();

					if (writes.size() == 1 && !reads.isEmpty()) {
						for (SimpleName simpleName : reads) {
							if (!isFileUse(simpleName)) {
								return true;
							}
						}

						refactorFile(type, initializer, reads);

						result= false;
						return false;
					}
				}
			}

			return true;
		}

		private void refactorFile(final Type type, final Expression initializer, final List<SimpleName> fileUses) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.NIORatherThanIOCleanUp_description);

			ClassInstanceCreation classInstanceCreation= ASTNodes.as(initializer, ClassInstanceCreation.class);

			String pathsName= addImport(Paths.class, classesToUseWithImport, importsToAdd);
			String pathName= addImport(Path.class, classesToUseWithImport, importsToAdd);
			ASTNodes.replaceButKeepComment(rewrite, type, ast.type(pathName), group);

			MethodInvocation getMethod= ast.newMethodInvocation();
			getMethod.setExpression(ASTNodeFactory.newName(ast, pathsName));
			getMethod.setName(ast.newSimpleName(GET_METHOD));
			getMethod.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression((Expression) classInstanceCreation.arguments().get(0))));

			ASTNodes.replaceButKeepComment(rewrite, classInstanceCreation, getMethod, group);

			for (SimpleName fileUse : fileUses) {
				MethodInvocation methodInvocation= (MethodInvocation) fileUse.getParent();

				if (ASTNodes.usesGivenSignature(methodInvocation, File.class.getCanonicalName(), TOPATH_METHOD)) {
					ASTNodes.replaceButKeepComment(rewrite, methodInvocation, ASTNodes.createMoveTarget(rewrite, fileUse), group);
				} else {
					MethodInvocation newMethodInvocation= ast.newMethodInvocation();
					newMethodInvocation.setExpression(ASTNodes.createMoveTarget(rewrite, fileUse));
					newMethodInvocation.setName(ast.newSimpleName(TOURI_METHOD));
					ASTNodes.replaceButKeepComment(rewrite, methodInvocation, newMethodInvocation, group);
				}
			}
		}
	}

	private boolean isFileCreation(final Expression initializer) {
		ClassInstanceCreation classInstanceCreation= ASTNodes.as(initializer, ClassInstanceCreation.class);

		return classInstanceCreation != null
				&& classInstanceCreation.getExpression() == null
				&& classInstanceCreation.getAnonymousClassDeclaration() == null
				&& ASTNodes.hasType(classInstanceCreation.resolveTypeBinding(), File.class.getCanonicalName())
				&& Utils.isEmpty(classInstanceCreation.typeArguments())
				&& classInstanceCreation.arguments() != null
				&& classInstanceCreation.arguments().size() == 1
				&& ASTNodes.hasType((Expression) classInstanceCreation.arguments().get(0), String.class.getCanonicalName());
	}

	private boolean isFileUse(final Expression fileObject) {
		if (fileObject != null && fileObject.getParent() instanceof MethodInvocation) {
			MethodInvocation methodInvocation= (MethodInvocation) fileObject.getParent();

			if (fileObject.getLocationInParent() == MethodInvocation.EXPRESSION_PROPERTY
					&& Utils.isEmpty(methodInvocation.arguments())
					&& (ASTNodes.usesGivenSignature(methodInvocation, File.class.getCanonicalName(), TOPATH_METHOD)
							|| ASTNodes.usesGivenSignature(methodInvocation, File.class.getCanonicalName(), TO_U_R_I_METHOD))) {
				return true;
			}
		}

		return false;
	}
}
