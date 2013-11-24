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

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Iterator;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.IRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.core.dom.*;
import static org.autorefactor.AutoRefactorPlugin.*;
import static org.autorefactor.refactoring.ASTHelper.*;

/** 
 * Aggregates running several visitors into only one visitor to increase performances.
 * When one visitor refactors a subtree of the AST, visitors coming after will not be able to visit it. 
 * Visitors throwing exceptions are isolated and ignored for the rest of a run for stability.
 */
public class AggregateASTVisitor extends ASTVisitor implements IJavaRefactoring {

	private final List<ASTVisitor> visitors;
	private RefactoringContext ctx;

	@SuppressWarnings("rawtypes")
	public AggregateASTVisitor(List<IRefactoring> visitors) {
		this.visitors = (List) visitors;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
		for (IRefactoring v : (List<IRefactoring>) (List) visitors) {
			v.setRefactoringContext(ctx);
		}
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}

	/**
	 * Generates the code for all the ASTVisitor methods that delegate to the underlying visitors.
	 */
	public static void main(String[] args) {
		final Method[] mm = ASTVisitor.class.getDeclaredMethods();
		Arrays.sort(mm, new Comparator<Method>() {

			public int compare(Method o1, Method o2) {
				return o1.getName().compareTo(o2.getName());
			}
		});
		for (Method m : mm) {
			System.out.println("@Override");
			System.out.print("public " + m.getReturnType() + " ");
			System.out.print(m.getName() + "(");
			Class<?>[] paramTypes = m.getParameterTypes();
			for (int i = 0; i < paramTypes.length; i++) {
				Class<?> paramType = paramTypes[i];
				if (i > 0) {
					System.out.print(", ");
				}
				System.out.print(paramType.getSimpleName() + " node");
			}
			System.out.println(") {");
			System.out.println("\tfor (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {");
			System.out.println("\t\tfinal ASTVisitor v = iter.next();");
			System.out.println("\t\ttry {");
			if (Boolean.TYPE.equals(m.getReturnType())) {
				System.out.println("\t\t\tfinal boolean continueVisiting = v." + m.getName() + "(node);");
				System.out.println("\t\t\tif (!continueVisiting) {");
				System.out.println("\t\t\t\t// changes will be made to this node.");
				System.out.println("\t\t\t\t// no other visitors can make any more changes to it");
				System.out.println("\t\t\t\t// => do not let other visitors visit this node");
				System.out.println("\t\t\t\treturn DO_NOT_VISIT_SUBTREE;");
				System.out.println("\t\t\t}");
			} else {
				System.out.println("\t\t\tv." + m.getName() + "(node);");
			}
			System.out.println("\t\t} catch (Exception e) {");
			System.out.println("\t\t\tfinal ILog log = AutoRefactorPlugin.getDefault().getLog();");
			System.out.println("\t\t\tlog.log(new Status(IStatus.ERROR, PLUGIN_ID,");
			System.out.println("\t\t\t\t\"Visitor \" + v.getClass().getName() + \" is faulty, it will be disabled for the rest of this run.\"));");
			System.out.println("\t\t\titer.remove();");
			System.out.println("\t\t}");
			System.out.println("\t}");
			if (Boolean.TYPE.equals(m.getReturnType())) {
				System.out.println("\treturn VISIT_SUBTREE;");
			}
			System.out.println("}");
			System.out.println();
		}
	}

	@Override
	public void endVisit(ExpressionStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(FieldAccess node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(EnumDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(FieldDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ForStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(IfStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ContinueStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(DoStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(EmptyStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(EnhancedForStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(EnumConstantDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(LabeledStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(LineComment node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(MarkerAnnotation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(MemberRef node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(MemberValuePair node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ImportDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(InfixExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(InstanceofExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(Initializer node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(Javadoc node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ArrayCreation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ArrayInitializer node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ArrayType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(AssertStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(Assignment node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(Block node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(WildcardType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(AnnotationTypeDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(AnnotationTypeMemberDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(AnonymousClassDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ArrayAccess node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(CharacterLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ClassInstanceCreation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(CompilationUnit node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ConditionalExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ConstructorInvocation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(BlockComment node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(BooleanLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(BreakStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(CastExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(CatchClause node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(SwitchStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(SynchronizedStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(TagElement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(TextElement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ThisExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ThrowStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(StringLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(SuperConstructorInvocation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(SuperFieldAccess node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(SuperMethodInvocation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(SwitchCase node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(UnionType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(VariableDeclarationExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(VariableDeclarationStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(VariableDeclarationFragment node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(WhileStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(TryStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(TypeDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(TypeDeclarationStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(TypeLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(TypeParameter node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(NormalAnnotation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(NullLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(NumberLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(PackageDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ParameterizedType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ParenthesizedExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(MethodRef node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(MethodRefParameter node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(MethodDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(MethodInvocation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(Modifier node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(ReturnStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(SimpleName node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(SimpleType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(SingleMemberAnnotation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(SingleVariableDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(PostfixExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(PrefixExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(PrimitiveType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(QualifiedName node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(QualifiedType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void postVisit(ASTNode node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.postVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public void preVisit(ASTNode node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				v.preVisit(node);
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
	}

	@Override
	public boolean preVisit2(ASTNode node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.preVisit2(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(EnumDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ExpressionStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(FieldAccess node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(FieldDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ForStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(IfStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ContinueStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(DoStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(EmptyStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(EnhancedForStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();

			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(EnumConstantDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(LabeledStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(LineComment node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MarkerAnnotation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MemberRef node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MemberValuePair node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ImportDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(InfixExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(InstanceofExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(Initializer node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(Javadoc node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ArrayCreation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ArrayInitializer node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ArrayType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(AssertStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(Assignment node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(Block node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(VariableDeclarationFragment node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(AnnotationTypeDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(AnnotationTypeMemberDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(AnonymousClassDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ArrayAccess node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(CharacterLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ClassInstanceCreation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(CompilationUnit node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ConditionalExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ConstructorInvocation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(BlockComment node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(BooleanLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(BreakStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(CastExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(CatchClause node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SwitchStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SynchronizedStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TagElement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TextElement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ThisExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ThrowStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(StringLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SuperConstructorInvocation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SuperFieldAccess node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SuperMethodInvocation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SwitchCase node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(UnionType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(VariableDeclarationExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(WildcardType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(WhileStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(VariableDeclarationStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TryStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TypeDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TypeDeclarationStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TypeLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(TypeParameter node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(NormalAnnotation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(NullLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(NumberLiteral node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(PackageDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ParameterizedType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ParenthesizedExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MethodRef node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MethodRefParameter node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MethodDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MethodInvocation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(Modifier node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(ReturnStatement node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SimpleName node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SimpleType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SingleMemberAnnotation node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(SingleVariableDeclaration node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(PostfixExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(PrefixExpression node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(PrimitiveType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(QualifiedName node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(QualifiedType node) {
		for (Iterator<ASTVisitor> iter = visitors.iterator(); iter.hasNext();) {
			final ASTVisitor v = iter.next();
			try {
				final boolean continueVisiting = v.visit(node);
				if (!continueVisiting) {
					// changes will be made to this node.
					// no other visitors can make any more changes to it
					// => do not let other visitors visit this node
					return DO_NOT_VISIT_SUBTREE;
				}
			} catch (Exception e) {
				final ILog log = AutoRefactorPlugin.getDefault().getLog();
				log.log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Visitor " + v.getClass().getName() + " is faulty, it will be disabled for the rest of this run."));
				iter.remove();
			}
		}
		return VISIT_SUBTREE;
	}

}
