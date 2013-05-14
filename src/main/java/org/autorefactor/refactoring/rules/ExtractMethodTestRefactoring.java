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

import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.internal.corext.dom.Bindings;

public class ExtractMethodTestRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private class Matcher extends ASTMatcher {
		// private Match fMatch;

		@Override
		public boolean match(SimpleName candidate, Object s) {
			if (s instanceof SimpleName) {
				return match(candidate, (SimpleName) s);
			} else if (s instanceof QualifiedName) {
				return match(candidate, (QualifiedName) s);
			} else if (s instanceof FieldAccess) {
				return match(candidate, (FieldAccess) s);
			}
			return false;
		}

		@Override
		public boolean match(QualifiedName candidate, Object s) {
			if (s instanceof SimpleName) {
				return match((SimpleName) s, candidate);
			} else if (s instanceof QualifiedName) {
				return match((QualifiedName) s, candidate);
			} else if (s instanceof FieldAccess) {
				return match((FieldAccess) s, candidate);
			}
			return false;
		}

		/**
		 * @see http
		 *      ://help.eclipse.org/helios/index.jsp?topic=%2Forg.eclipse.jdt
		 *      .doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fjdt%2Fcore%2Fdom%2F
		 *      FieldAccess.html
		 */
		@Override
		public boolean match(FieldAccess candidate, Object s) {
			if (s instanceof SimpleName) {
				// TODO JNR not correct, need to have another method for this
				return match((SimpleName) s, candidate);
			} else if (s instanceof QualifiedName) {
				// TODO JNR do something about this?
				return match(candidate, (QualifiedName) s);
			} else if (s instanceof FieldAccess) {
				// TODO JNR do something about this?
				return match(candidate, (FieldAccess) s);
			}
			return false;
		}

		private boolean match(Expression candidate, Expression snippet) {
			if (candidate instanceof ThisExpression) {
				return snippet instanceof ThisExpression;
			} else if (candidate instanceof SimpleName) {
				final SimpleName sn = (SimpleName) candidate;
				if (snippet instanceof SimpleName) {
					return match(sn, (SimpleName) snippet);
				} else if (snippet instanceof QualifiedName) {
					return match(sn, (QualifiedName) snippet);
				} else if (snippet instanceof FieldAccess) {
					return match(sn, (FieldAccess) snippet);
				}
			} else if (candidate instanceof QualifiedName) {
				final QualifiedName qn = (QualifiedName) candidate;
				if (snippet instanceof SimpleName) {
					return match((SimpleName) snippet, qn);
				} else if (snippet instanceof QualifiedName) {
					return match(qn, (QualifiedName) snippet);
				} else if (snippet instanceof FieldAccess) {
					return match((FieldAccess) snippet, qn);
				}
			} else if (candidate instanceof FieldAccess) {
				final FieldAccess fa = (FieldAccess) candidate;
				if (snippet instanceof SimpleName) {
					return match((SimpleName) snippet, fa);
				} else if (snippet instanceof QualifiedName) {
					return match(fa, (QualifiedName) snippet);
				} else if (snippet instanceof FieldAccess) {
					return match(fa, (FieldAccess) snippet);
				}
			}
			return false;
		}

		public boolean match(SimpleName candidate, SimpleName snippet) {
			if (candidate.isDeclaration() != snippet.isDeclaration())
				return false;

			IBinding cb = candidate.resolveBinding();
			IBinding sb = snippet.resolveBinding();
			if (cb == null || sb == null)
				return false;
			IVariableBinding vcb = ASTNodes.getVariableBinding(candidate);
			IVariableBinding vsb = ASTNodes.getVariableBinding(snippet);
			if (vcb == null || vsb == null)
				return Bindings.equals(cb, sb);
			if (!vcb.isField() && !vsb.isField()
					&& Bindings.equals(vcb.getType(), vsb.getType())) {
				// SimpleName mapped = fMatch.getMappedName(vsb);
				// if (mapped != null) {
				// IVariableBinding mappedBinding = ASTNodes
				// .getVariableBinding(mapped);
				// if (!Bindings.equals(vcb, mappedBinding))
				// return false;
				// }
				// fMatch.addLocal(vsb, candidate);
				return true;
			}
			return Bindings.equals(cb, sb);
		}

		public boolean match(SimpleName candidate, QualifiedName snippet) {
			return false;
		}

		public boolean match(SimpleName candidate, FieldAccess snippet) {
			if (!(snippet.getExpression() instanceof ThisExpression)) {
				// TODO JNR parenthesized expr??
				return false;
			}
			IBinding cb = candidate.resolveBinding();
			IBinding sb = snippet.resolveFieldBinding();
			if (cb == null || sb == null)
				return false;
			return Bindings.equals(cb, sb);
		}

		public boolean match(QualifiedName candidate, QualifiedName snippet) {
			IBinding cb = candidate.resolveBinding();
			IBinding sb = snippet.resolveBinding();
			if (cb == null || sb == null)
				return false;
			if (cb.isEqualTo(sb)) {
				return match(candidate.getQualifier(), snippet.getQualifier());
			}
			return false;
		}

		public boolean match(FieldAccess candidate, QualifiedName snippet) {
			IBinding cb = candidate.resolveFieldBinding();
			IBinding sb = snippet.resolveBinding();
			if (cb == null || sb == null)
				return false;
			if (cb.isEqualTo(sb)) {
				return match(candidate.getExpression(), snippet.getQualifier());
			}
			return false;
		}

		public boolean match(FieldAccess candidate, FieldAccess snippet) {
			IBinding cb = candidate.resolveFieldBinding();
			IBinding sb = snippet.resolveFieldBinding();
			if (cb == null || sb == null)
				return false;
			if (Bindings.equals(cb, sb)) {
				return match(candidate.getExpression(), snippet.getExpression());
			}
			return false;
		}

	}

	private RefactoringContext ctx;

	public ExtractMethodTestRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
	}

	@Override
	/**
	 * @see https://bugs.eclipse.org/bugs/show_bug.cgi?id=58648
	 * @see org.eclipse.jdt.internal.corext.refactoring.code.SnippetFinder.Matcher.match()
	 */
	public boolean visit(org.eclipse.jdt.core.dom.Block node) {
		// TODO JNR remove
		List<Statement> stmts = node.statements();
		if (stmts.size() == 14) {
			int idx = 1;

			final List<ASTNode> operands = new ArrayList<ASTNode>();
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			operands.add(getOperand(stmts.get(idx++)));
			final Block b = (Block) stmts.get(idx++);
			operands.add(getOperand((ExpressionStatement) b.statements().get(1)));

			try {
				PrintStream os = new PrintStream(
						"C:/Users/jnrouvignac/AutoRefactor/target/extract_method_test.html");
				os.println("<html>");
				os.println("<head><style>table { border:1px solid black;  border-collapse:collapse; } td { border:1px solid black; }</style></head>");
				os.println("<body><table>");
				os.println("  <tr><td></td>");
				for (ASTNode op : operands) {
					os.print("    <td>");
					os.print(op);
					os.print("</td>");
				}
				os.println("  </tr>");
				final Matcher m = new Matcher();
				for (int i = 0; i < operands.size(); i++) {
					ASTNode op1 = operands.get(i);
					os.print("  <tr><td>");
					os.print("(" + op1.getClass().getSimpleName() + ") " + op1);
					os.print("</td>");
					for (int j = 0; j < operands.size(); j++) {
						ASTNode op2 = operands.get(j);
						final boolean isMatch = isMatch(m, op1, op2);
						os.print("<td bgcolor='" + (isMatch ? "lime" : "red")
								+ "'>");
						os.print(isMatch);
						os.print("</td>");
					}
					os.println("  </tr>");
				}
				os.println("</table></body></html>");
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			}

			int i = 0;
		}
		return true;
	}

	private boolean isMatch(final Matcher m, ASTNode op1, ASTNode op2) {
		if (op1 instanceof SimpleName) {
			return m.match((SimpleName) op1, op2);
		} else if (op1 instanceof QualifiedName) {
			return m.match((QualifiedName) op1, op2);
		} else if (op1 instanceof FieldAccess) {
			return m.match((FieldAccess) op1, op2);
		}
		return false;
	}

	private IBinding resolveBinding(ASTNode op) {
		if (op instanceof Name) {
			return ((Name) op).resolveBinding();
		} else if (op instanceof FieldAccess) {
			return ((FieldAccess) op).resolveFieldBinding();
		}
		return null;
	}

	private Expression getOperand(Statement s) {
		Assignment pe = (Assignment) ((ExpressionStatement) s).getExpression();
		return pe.getRightHandSide();
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}
