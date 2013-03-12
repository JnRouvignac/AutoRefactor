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
package org.autorefactor.cfg;

import org.autorefactor.refactoring.ASTHelper;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.ConstructorInvocation;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.MemberRef;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.MethodRef;
import org.eclipse.jdt.core.dom.MethodRefParameter;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.QualifiedType;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.SynchronizedStatement;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TextElement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclarationStatement;
import org.eclipse.jdt.core.dom.TypeLiteral;
import org.eclipse.jdt.core.dom.TypeParameter;
import org.eclipse.jdt.core.dom.UnionType;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jdt.core.dom.WildcardType;

public class CFGASTVisitor extends ASTVisitor {

	@Override
	public boolean visit(QualifiedName node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(PrimitiveType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(QualifiedType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(PrefixExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(PostfixExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ParenthesizedExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SingleVariableDeclaration node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SimpleType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SimpleName node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ReturnStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(Modifier node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MethodInvocation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MethodDeclaration node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MethodRefParameter node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MethodRef node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MemberValuePair node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ParameterizedType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(PackageDeclaration node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(NumberLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(NullLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(UnionType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TypeParameter node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TypeLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TypeDeclarationStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TypeDeclaration node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TryStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(WildcardType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(WhileStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(VariableDeclarationFragment node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(VariableDeclarationStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(VariableDeclarationExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SwitchStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SwitchCase node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SuperMethodInvocation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SuperFieldAccess node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SuperConstructorInvocation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(StringLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ThrowStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ThisExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TextElement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TagElement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SynchronizedStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(CatchClause node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(CastExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(BreakStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(BooleanLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ConstructorInvocation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ConditionalExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(CompilationUnit node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ClassInstanceCreation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(CharacterLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ArrayCreation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ArrayAccess node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(AnonymousClassDeclaration node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(Block node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(Assignment node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(AssertStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ArrayType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ArrayInitializer node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(Initializer node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(InstanceofExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(InfixExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ImportDeclaration node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(IfStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MemberRef node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(LabeledStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(EnhancedForStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(EmptyStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(DoStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ContinueStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ForStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(FieldDeclaration node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(FieldAccess node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ExpressionStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return ASTHelper.VISIT_SUBTREE;
	}

}
