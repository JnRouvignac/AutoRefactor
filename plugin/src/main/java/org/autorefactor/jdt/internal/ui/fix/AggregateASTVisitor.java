/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
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

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.JavaRefactoringRule;
import org.autorefactor.jdt.internal.corext.dom.RefactoringRule;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.autorefactor.preferences.Preferences;
import org.autorefactor.util.AutoRefactorException;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.UnhandledException;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeMemberDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BlockComment;
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
import org.eclipse.jdt.core.dom.CreationReference;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.EnumConstantDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.ExpressionMethodReference;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.MarkerAnnotation;
import org.eclipse.jdt.core.dom.MemberRef;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.MethodRef;
import org.eclipse.jdt.core.dom.MethodRefParameter;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NormalAnnotation;
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
import org.eclipse.jdt.core.dom.SingleMemberAnnotation;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SuperMethodReference;
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
import org.eclipse.jdt.core.dom.TypeMethodReference;
import org.eclipse.jdt.core.dom.TypeParameter;
import org.eclipse.jdt.core.dom.UnionType;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jdt.core.dom.WildcardType;

/**
 * Aggregates running several visitors into only one visitor to increase
 * performances. When one visitor refactors a subtree of the AST, visitors
 * coming after will not be able to visit it. Visitors throwing exceptions are
 * isolated and ignored for the rest of a run for stability.
 */
public class AggregateASTVisitor extends ASTVisitor implements JavaRefactoringRule {
	private final Map<Class<?>, List<ASTVisitor>> visitorsMap= new HashMap<>();
	private final Map<Class<?>, List<ASTVisitor>> endVisitorsMap= new HashMap<>();
	private final Set<ASTVisitor> preVisitors= new LinkedHashSet<>();
	private final Set<ASTVisitor> preVisitors2= new LinkedHashSet<>();
	private final Set<ASTVisitor> postVisitors= new LinkedHashSet<>();

	private final List<ASTVisitor> visitors;

	private CompilationUnitRewrite cuRewrite;
	private final Set<ASTVisitor> visitorsContributingRefactoring= new HashSet<>();

	/**
	 * Builds an instance of this class.
	 *
	 * @param visitors the visitors that will be executed by this
	 *                 {@link AggregateASTVisitor}
	 */
	@SuppressWarnings({ "rawtypes" })
	public AggregateASTVisitor(final List<RefactoringRule> visitors) {
		this.visitors= (List) visitors;
		analyzeVisitors();
	}

	@Override
	public String getName() {
		throw new UnsupportedOperationException();
	}

	@Override
	public String getDescription() {
		throw new UnsupportedOperationException();
	}

	@Override
	public String getReason() {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean isEnabled(final Preferences preferences) {
		return true;
	}

	private void analyzeVisitors() {
		for (ASTVisitor v : this.visitors) {
			analyzeVisitor(v, v.getClass());
		}
	}

	private void analyzeVisitor(final ASTVisitor v, final Class<?> clazz) {
		if (ASTVisitor.class.equals(clazz)) {
			return;
		}
		for (Method m : clazz.getDeclaredMethods()) {
			if (is("preVisit", m)) { //$NON-NLS-1$
				preVisitors.add(v);
			} else if (is("preVisit2", m)) { //$NON-NLS-1$
				preVisitors2.add(v);
			} else if (is("postVisit", m)) { //$NON-NLS-1$
				postVisitors.add(v);
			} else if (isVisit(m)) {
				put(visitorsMap, m.getParameterTypes()[0], v);
			} else if (isEndVisit(m)) {
				put(endVisitorsMap, m.getParameterTypes()[0], v);
			}
		}
		analyzeVisitor(v, clazz.getSuperclass());
	}

	private static boolean is(final String methodName, final Method m) {
		return methodName.equals(m.getName()) && m.getParameterTypes().length == 1
				&& ASTNode.class.equals(m.getParameterTypes()[0]);
	}

	private static boolean isVisit(final Method m) {
		return "visit".equals(m.getName()) && m.getParameterTypes().length == 1 //$NON-NLS-1$
				&& ASTNode.class.isAssignableFrom(m.getParameterTypes()[0])
				&& !Modifier.isAbstract(m.getParameterTypes()[0].getModifiers());
	}

	private static boolean isEndVisit(final Method m) {
		return "endVisit".equals(m.getName()) && m.getParameterTypes().length == 1 //$NON-NLS-1$
				&& ASTNode.class.isAssignableFrom(m.getParameterTypes()[0])
				&& !Modifier.isAbstract(m.getParameterTypes()[0].getModifiers());
	}

	private void put(final Map<Class<?>, List<ASTVisitor>> map, final Class<?> key, final ASTVisitor value) {
		List<ASTVisitor> visitors= map.get(key);
		if (visitors == null) {
			visitors= new ArrayList<>(1);
			map.put(key, visitors);
		}
		visitors.add(value);
	}

	private List<ASTVisitor> getVisitors(final Map<Class<?>, List<ASTVisitor>> map,
			final Class<? extends ASTNode> clazzKey) {
		List<ASTVisitor> result= map.get(clazzKey);
		if (result != null) {
			return result;
		}

		return Collections.emptyList();
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return true;
	}

	private boolean isJavaVersionSupported(final ASTVisitor visitor) {
		Release javaSERelease= cuRewrite.getJavaProjectOptions().getJavaSERelease();
		return visitor instanceof JavaRefactoringRule
				&& ((JavaRefactoringRule) visitor).isJavaVersionSupported(javaSERelease);
	}

	@Override
	public void setRefactoringContext(final CompilationUnitRewrite cuRewrite) {
		this.cuRewrite= cuRewrite;

		for (ASTVisitor v : visitors) {
			((RefactoringRule) v).setRefactoringContext(cuRewrite);
		}

		this.visitorsContributingRefactoring.clear();
	}

	@Override
	public ASTRewrite getRefactorings(final CompilationUnit astRoot) {
		astRoot.accept(this);
		return cuRewrite.getASTRewrite();
	}

	/**
	 * Get the cleanups.
	 *
	 * @return the cleanups.
	 */
	public ASTRewrite getRefactorings() {
		return cuRewrite.getASTRewrite();
	}

	/**
	 * Returns the visitors that contributed cleanups in the last run.
	 *
	 * @return the visitors that contributed cleanups in the last run
	 */
	public Set<ASTVisitor> getVisitorsContributingRefactoring() {
		return visitorsContributingRefactoring;
	}

	/**
	 * Verify whether the following visitors can visit the current node.
	 *
	 * @param continueVisiting whether the current visitor reported it wants to
	 *                         visit the subtree of the current node
	 * @param v                the current visitor
	 * @param node             the node being currently visited
	 * @return true if the following visitors can visit the current node, false
	 *         otherwise
	 */
	private boolean continueVisiting(final boolean continueVisiting, final ASTVisitor v, final ASTNode node) {
		if (!continueVisiting) {
			if (!cuRewrite.getASTRewrite().hasRefactorings()) {
				logBadlyBehavedVisitor(v, node);
			} else {
				visitorsContributingRefactoring.add(v);
			}
			// Changes will be made to this node.
			// no other visitors can make any more changes to it
			// => do not let other visitors visit this node
			return false;
		}

		return true;
	}

	private void logBadlyBehavedVisitor(final ASTVisitor v, final ASTNode node) {
		String message= "Visitor " + v.getClass().getName() + " is badly behaved:" //$NON-NLS-1$ //$NON-NLS-2$
				+ " it reported doing a refactoring, but it did not actually contribute any refactoring."; //$NON-NLS-1$
		cuRewrite.getLogger().error(message, new AutoRefactorException(node, message));
	}

	private void logFaultyVisitor(final ASTVisitor v, final ASTNode node, final Exception e) {
		if (e instanceof OperationCanceledException) {
			// Let the user cancel the current operation
			throw (OperationCanceledException) e;
		}
		String message= "Visitor " + v.getClass().getName() + " is faulty," //$NON-NLS-1$ //$NON-NLS-2$
				+ " it will be disabled for the rest of this run."; //$NON-NLS-1$
		cuRewrite.getLogger().error(message, new UnhandledException(node, message, e));
	}

	/**
	 * Generates the code for all the ASTVisitor methods that delegate to the
	 * underlying visitors.
	 *
	 * @param args the arguments of the Java program
	 */
	public static void main(final String[] args) {
		Method[] mm= ASTVisitor.class.getDeclaredMethods();
		Arrays.sort(mm, Comparator.comparing(Method::getName));
		for (Method m : mm) {
			System.out.println("@Override"); //$NON-NLS-1$
			System.out.print("public " + m.getReturnType() + " "); //$NON-NLS-1$ //$NON-NLS-2$
			System.out.print(m.getName() + "("); //$NON-NLS-1$
			Class<?>[] paramTypes= m.getParameterTypes();
			for (int i= 0; i < paramTypes.length; i++) {
				Class<?> paramType= paramTypes[i];
				if (i > 0) {
					System.out.print(", "); //$NON-NLS-1$
				}
				System.out.print(paramType.getSimpleName() + " node"); //$NON-NLS-1$
			}
			System.out.println(") {"); //$NON-NLS-1$
			boolean isVisit= isVisit(m);
			boolean isEndVisit= isEndVisit(m);
			boolean isPrevisit2= is("preVisit2", m); //$NON-NLS-1$
			if (isVisit || isEndVisit) {
				System.out.print("\tfinal List<ASTVisitor> visitorList = getVisitors("); //$NON-NLS-1$
				System.out.print((isVisit ? "visitorsMap" : "endVisitorsMap") + ", "); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				System.out.println(m.getParameterTypes()[0].getSimpleName() + ".class);"); //$NON-NLS-1$
			}
			System.out.print("\tfor (Iterator<ASTVisitor> iter = "); //$NON-NLS-1$
			if (is("preVisit", m)) { //$NON-NLS-1$
				System.out.print("preVisitors"); //$NON-NLS-1$
			} else if (isPrevisit2) {
				System.out.print("preVisitors2"); //$NON-NLS-1$
			} else if (is("postVisit", m)) { //$NON-NLS-1$
				System.out.print("postVisitors"); //$NON-NLS-1$
			} else if (isVisit || isEndVisit) {
				System.out.print("visitorList"); //$NON-NLS-1$
			} else {
				throw new NotImplementedException(null, "for method " + m); //$NON-NLS-1$
			}
			System.out.println(".iterator(); iter.hasNext();) {"); //$NON-NLS-1$
			System.out.println("\t\tfinal ASTVisitor v = iter.next();"); //$NON-NLS-1$
			System.out.println("\t\ttry {"); //$NON-NLS-1$
			if (isPrevisit2) {
				System.out.println("\t\t\tif (!v." + m.getName() + "(node)) {"); //$NON-NLS-1$ //$NON-NLS-2$
				System.out.println("\t\t\t\treturn DO_NOT_VISIT_SUBTREE;"); //$NON-NLS-1$
				System.out.println("\t\t\t}"); //$NON-NLS-1$
			} else if (Boolean.TYPE.equals(m.getReturnType())) {
				System.out.println("\t\t\tif (isJavaVersionSupported(v)"); //$NON-NLS-1$
				System.out.println("\t\t\t\t\t&& !continueVisiting(v." + m.getName() + "(node), v, node)) {"); //$NON-NLS-1$ //$NON-NLS-2$
				System.out.println("\t\t\t\treturn DO_NOT_VISIT_SUBTREE;"); //$NON-NLS-1$
				System.out.println("\t\t\t}"); //$NON-NLS-1$
			} else {
				System.out.println("\t\t\tv." + m.getName() + "(node);"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			System.out.println("\t\t} catch (Exception e) {"); //$NON-NLS-1$
			System.out.println("\t\t\tlogFaultyVisitor(v, node, e);"); //$NON-NLS-1$
			System.out.println("\t\t\titer.remove();"); //$NON-NLS-1$
			System.out.println("\t\t}"); //$NON-NLS-1$
			System.out.println("\t}"); //$NON-NLS-1$
			if (Boolean.TYPE.equals(m.getReturnType())) {
				System.out.println("\treturn VISIT_SUBTREE;"); //$NON-NLS-1$
			}
			System.out.println("}"); //$NON-NLS-1$
			System.out.println();
		}
	}

	@Override
	public void endVisit(final AnnotationTypeDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, AnnotationTypeDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final AnnotationTypeMemberDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, AnnotationTypeMemberDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final AnonymousClassDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, AnonymousClassDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ArrayAccess node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ArrayAccess.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ArrayCreation node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ArrayCreation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ArrayInitializer node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ArrayInitializer.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ArrayType node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ArrayType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final AssertStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, AssertStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final Assignment node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, Assignment.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final Block node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, Block.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final BlockComment node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, BlockComment.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final BooleanLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, BooleanLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final BreakStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, BreakStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final CastExpression node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, CastExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final CatchClause node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, CatchClause.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final CharacterLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, CharacterLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ClassInstanceCreation node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ClassInstanceCreation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final CompilationUnit node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, CompilationUnit.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ConditionalExpression node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ConditionalExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ConstructorInvocation node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ConstructorInvocation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ContinueStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ContinueStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final DoStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, DoStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final EmptyStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, EmptyStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final EnhancedForStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, EnhancedForStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final EnumConstantDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, EnumConstantDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final EnumDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, EnumDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ExpressionStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ExpressionStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final FieldAccess node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, FieldAccess.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final FieldDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, FieldDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ForStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ForStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final IfStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, IfStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ImportDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ImportDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final InfixExpression node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, InfixExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final Initializer node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, Initializer.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final InstanceofExpression node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, InstanceofExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final Javadoc node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, Javadoc.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final LabeledStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, LabeledStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final LineComment node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, LineComment.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final MarkerAnnotation node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, MarkerAnnotation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final MemberRef node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, MemberRef.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final MemberValuePair node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, MemberValuePair.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final MethodDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, MethodDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final MethodInvocation node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, MethodInvocation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final MethodRef node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, MethodRef.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final MethodRefParameter node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, MethodRefParameter.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final Modifier node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, Modifier.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final NormalAnnotation node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, NormalAnnotation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final NullLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, NullLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final NumberLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, NumberLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final PackageDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, PackageDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ParameterizedType node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ParameterizedType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ParenthesizedExpression node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ParenthesizedExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final PostfixExpression node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, PostfixExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final PrefixExpression node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, PrefixExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final PrimitiveType node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, PrimitiveType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final QualifiedName node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, QualifiedName.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final QualifiedType node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, QualifiedType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ReturnStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ReturnStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final SimpleName node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, SimpleName.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final SimpleType node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, SimpleType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final SingleMemberAnnotation node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, SingleMemberAnnotation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final SingleVariableDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, SingleVariableDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final StringLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, StringLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final SuperConstructorInvocation node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, SuperConstructorInvocation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final SuperFieldAccess node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, SuperFieldAccess.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final SuperMethodInvocation node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, SuperMethodInvocation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final SwitchCase node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, SwitchCase.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final SwitchStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, SwitchStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final SynchronizedStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, SynchronizedStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final TagElement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, TagElement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final TextElement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, TextElement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ThisExpression node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ThisExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final ThrowStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, ThrowStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final TryStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, TryStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final TypeDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, TypeDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final TypeDeclarationStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, TypeDeclarationStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final TypeLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, TypeLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final TypeParameter node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, TypeParameter.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final UnionType node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, UnionType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final VariableDeclarationExpression node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, VariableDeclarationExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final VariableDeclarationFragment node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, VariableDeclarationFragment.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final VariableDeclarationStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, VariableDeclarationStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final WhileStatement node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, WhileStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void endVisit(final WildcardType node) {
		List<ASTVisitor> visitorList= getVisitors(endVisitorsMap, WildcardType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.endVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void postVisit(final ASTNode node) {
		for (Iterator<ASTVisitor> iter= postVisitors.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.postVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public void preVisit(final ASTNode node) {
		for (Iterator<ASTVisitor> iter= preVisitors.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				v.preVisit(node);
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}
	}

	@Override
	public boolean preVisit2(final ASTNode node) {
		for (Iterator<ASTVisitor> iter= preVisitors2.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (!v.preVisit2(node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final AnnotationTypeDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, AnnotationTypeDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final AnnotationTypeMemberDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, AnnotationTypeMemberDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final AnonymousClassDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, AnonymousClassDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ArrayAccess node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ArrayAccess.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ArrayCreation node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ArrayCreation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ArrayInitializer node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ArrayInitializer.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ArrayType node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ArrayType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final AssertStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, AssertStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final Assignment node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, Assignment.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final Block node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, Block.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final BlockComment node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, BlockComment.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final BooleanLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, BooleanLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final BreakStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, BreakStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final CastExpression node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, CastExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final CatchClause node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, CatchClause.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final CharacterLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, CharacterLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ClassInstanceCreation node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ClassInstanceCreation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final CompilationUnit node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, CompilationUnit.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ConditionalExpression node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ConditionalExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ConstructorInvocation node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ConstructorInvocation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ContinueStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ContinueStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final CreationReference node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, CreationReference.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final DoStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, DoStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final EmptyStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, EmptyStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final EnhancedForStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, EnhancedForStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final EnumConstantDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, EnumConstantDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final EnumDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, EnumDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ExpressionMethodReference node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ExpressionMethodReference.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ExpressionStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ExpressionStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final FieldAccess node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, FieldAccess.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final FieldDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, FieldDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ForStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ForStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final IfStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, IfStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ImportDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ImportDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final InfixExpression node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, InfixExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final Initializer node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, Initializer.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final InstanceofExpression node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, InstanceofExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final Javadoc node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, Javadoc.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final LabeledStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, LabeledStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final LambdaExpression node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, LambdaExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final LineComment node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, LineComment.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final MarkerAnnotation node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, MarkerAnnotation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final MemberRef node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, MemberRef.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final MemberValuePair node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, MemberValuePair.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final MethodDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, MethodDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, MethodInvocation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final MethodRef node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, MethodRef.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final MethodRefParameter node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, MethodRefParameter.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final Modifier node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, Modifier.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final NormalAnnotation node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, NormalAnnotation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final NullLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, NullLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final NumberLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, NumberLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final PackageDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, PackageDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ParameterizedType node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ParameterizedType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ParenthesizedExpression node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ParenthesizedExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final PostfixExpression node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, PostfixExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final PrefixExpression node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, PrefixExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final PrimitiveType node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, PrimitiveType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final QualifiedName node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, QualifiedName.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final QualifiedType node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, QualifiedType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ReturnStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ReturnStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SimpleName node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SimpleName.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SimpleType node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SimpleType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SingleMemberAnnotation node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SingleMemberAnnotation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SingleVariableDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SingleVariableDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final StringLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, StringLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SuperConstructorInvocation node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SuperConstructorInvocation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SuperFieldAccess node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SuperFieldAccess.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SuperMethodInvocation node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SuperMethodInvocation.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SuperMethodReference node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SuperMethodReference.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SwitchCase node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SwitchCase.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SwitchStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SwitchStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final SynchronizedStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, SynchronizedStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final TagElement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, TagElement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final TextElement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, TextElement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ThisExpression node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ThisExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ThrowStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, ThrowStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final TryStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, TryStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final TypeDeclaration node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, TypeDeclaration.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final TypeDeclarationStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, TypeDeclarationStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final TypeLiteral node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, TypeLiteral.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final TypeMethodReference node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, TypeMethodReference.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final TypeParameter node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, TypeParameter.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final UnionType node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, UnionType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final VariableDeclarationExpression node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, VariableDeclarationExpression.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final VariableDeclarationFragment node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, VariableDeclarationFragment.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final VariableDeclarationStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, VariableDeclarationStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final WhileStatement node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, WhileStatement.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final WildcardType node) {
		List<ASTVisitor> visitorList= getVisitors(visitorsMap, WildcardType.class);
		for (Iterator<ASTVisitor> iter= visitorList.iterator(); iter.hasNext();) {
			ASTVisitor v= iter.next();
			try {
				if (isJavaVersionSupported(v) && !continueVisiting(v.visit(node), v, node)) {
					return false;
				}
			} catch (Exception e) {
				logFaultyVisitor(v, node, e);
				iter.remove();
			}
		}

		return true;
	}

	@Override
	public boolean isByDefault() {
		return false;
	}
}
