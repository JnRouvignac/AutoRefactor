/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Zsombor Gegesy - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix;

import static org.eclipse.jdt.core.dom.ASTNode.ANNOTATION_TYPE_DECLARATION;
import static org.eclipse.jdt.core.dom.ASTNode.ANONYMOUS_CLASS_DECLARATION;
import static org.eclipse.jdt.core.dom.ASTNode.ENUM_DECLARATION;
import static org.eclipse.jdt.core.dom.ASTNode.TYPE_DECLARATION;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.CollectorVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.jdt.internal.corext.dom.Variable;
import org.autorefactor.util.IllegalStateException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class EntrySetRatherThanKeySetAndValueSearchCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_EntrySetRatherThanKeySetAndValueSearchCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_EntrySetRatherThanKeySetAndValueSearchCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_EntrySetRatherThanKeySetAndValueSearchCleanUp_reason;
    }

    /**
     * This class helps decide which name to give to a new variable.
     * <p>
     * When creating a new variable, its name may shadow another variable or field
     * used in the local scope.
     * <p>
     * Does JDT provide a public API for naming local variables? I could not find
     * any.
     */
    private static final class VariableNameDecider {
        private final ASTNode scope;
        private final int insertionPoint;
        private final ASTNode namingScope;

        private VariableNameDecider(final ASTNode scope, final int insertionPoint) {
            this.scope= scope;
            this.insertionPoint= insertionPoint;
            this.namingScope= getNamingScope(scope);
        }

        private ASTNode getNamingScope(final ASTNode scope) {
            Class<?>[] ancestorClasses= { MethodDeclaration.class, Initializer.class };
            ASTNode ancestor= ASTNodes.getFirstAncestorOrNull(scope, ancestorClasses);
            if (ancestor == null) {
                throw new IllegalStateException(scope, "Expected to find an ancestor among the types " //$NON-NLS-1$
                        + Arrays.toString(ancestorClasses) + " but could not find any"); //$NON-NLS-1$
            }

            return ancestor;
        }

        /**
         * Returns a name suggestion suitable for use when inserting a new variable
         * declaration. This name:
         * <ul>
         * <li>will not shadow any variable name in use after the insertion point</li>
         * <li>and will not conflict with local variables declared before the insertion
         * point.</li>
         * </ul>
         *
         * @param candidateNames the suggestion will be one of the candidate names,
         *                       maybe suffixed by a number
         * @return the suggestion for a variable name
         */
        public String suggest(final String... candidateNames) {
            Set<String> declaredLocalVarNames= new HashSet<>(collectDeclaredLocalVariableNames());
            Set<String> varNamesUsedAfter= new HashSet<>(collectVariableNamesUsedAfter());
            // Can we use one of the candidate names?
            for (String candidate : candidateNames) {
                if (isSuitable(candidate, declaredLocalVarNames, varNamesUsedAfter)) {
                    return candidate;
                }
            }

            // Iterate on the first candidate name and suffix it with an integer
            int i= 1;
            do {
                String candidate= candidateNames[0] + i;
                if (isSuitable(candidate, declaredLocalVarNames, varNamesUsedAfter)) {
                    return candidate;
                }
                i++;
            } while (true);
        }

        private boolean isSuitable(final String candidateName, final Set<String> declaredLocalVarNames,
                final Set<String> varNamesUsedAfter) {
            // No variable declaration conflict
            return !declaredLocalVarNames.contains(candidateName)
                    // New variable does not shadow use of other variables/fields with the same name
                    && !varNamesUsedAfter.contains(candidateName);
        }

        private Collection<String> collectDeclaredLocalVariableNames() {
            return new CollectorVisitor<String>() {
                @Override
                public boolean preVisit2(final ASTNode node) {
                    return !isTypeDeclaration(node);
                }

                @Override
                public boolean visit(final SimpleName node) {
                    IBinding binding= node.resolveBinding();
                    if (binding != null && binding.getKind() == IBinding.VARIABLE) {
                        addResult(binding.getName());
                    }

                    return true;
                }
            }.collect(namingScope);
        }

        private List<String> collectVariableNamesUsedAfter() {
            return new CollectorVisitor<String>() {
                @Override
                public boolean preVisit2(final ASTNode node) {
                    return node.getStartPosition() > insertionPoint && !isTypeDeclaration(node);
                }

                @Override
                public boolean visit(final SimpleName node) {
                    IBinding binding= node.resolveBinding();
                    if (binding != null && binding.getKind() == IBinding.VARIABLE) {
                        addResult(binding.getName());
                    }

                    return true;
                }
            }.collect(scope);
        }

        private boolean isTypeDeclaration(final ASTNode node) {
            switch (node.getNodeType()) {
            case ANNOTATION_TYPE_DECLARATION:
            case ANONYMOUS_CLASS_DECLARATION:
            case ENUM_DECLARATION:
            case TYPE_DECLARATION:
                return true;

            default:
                return false;
            }
        }
    }

    @Override
    public boolean visit(final EnhancedForStatement enhancedFor) {
        MethodInvocation foreachExpression= ASTNodes.as(enhancedFor.getExpression(), MethodInvocation.class);

        if (isKeySetMethod(foreachExpression)) {
            // From 'for (K key : map.keySet()) { }'
            // -> mapExpression become 'map', parameter become 'K key'
            Expression mapExpression= foreachExpression.getExpression();
            if (mapExpression == null) {
                // Not implemented
                return true;
            }
            SingleVariableDeclaration parameter= enhancedFor.getParameter();
            List<MethodInvocation> getValueMis= collectMapGetValueCalls(mapExpression, parameter,
                    enhancedFor.getBody());
            if (!getValueMis.isEmpty() && haveSameTypeBindings(getValueMis)) {
                replaceEntryIterationByKeyIteration(enhancedFor, mapExpression, parameter, getValueMis);
                return false;
            }
        }

        return true;
    }

    private void replaceEntryIterationByKeyIteration(final EnhancedForStatement enhancedFor, final Expression mapExpression,
            final SingleVariableDeclaration parameter, final List<MethodInvocation> getValueMis) {
        ASTNodeFactory b= cuRewrite.getASTBuilder();
        Refactorings r= cuRewrite.getRefactorings();

        VarDefinitionsUsesVisitor keyUseVisitor= new VarDefinitionsUsesVisitor(parameter);
        enhancedFor.getBody().accept(keyUseVisitor);
        int keyUses= keyUseVisitor.getReads().size();

        int insertionPoint= ASTNodes.asList(enhancedFor.getBody()).get(0).getStartPosition() - 1;
        Variable entryVar= new Variable(
                new VariableNameDecider(enhancedFor.getBody(), insertionPoint).suggest("entry", "mapEntry"), b); //$NON-NLS-1$ //$NON-NLS-2$
        TypeNameDecider typeNameDecider= new TypeNameDecider(parameter);

        MethodInvocation getValueMi0= getValueMis.get(0);
        ITypeBinding typeBinding= getValueMi0.getExpression().resolveTypeBinding();

        if (typeBinding != null && typeBinding.isRawType()) {
            // for (Object key : map.keySet()) => for (Object key : map.entrySet())
            r.set(enhancedFor, EnhancedForStatement.EXPRESSION_PROPERTY, b.invoke(b.createMoveTarget(mapExpression), "entrySet")); //$NON-NLS-1$
            Type objectType= b.type(typeNameDecider.useSimplestPossibleName(Object.class.getCanonicalName()));
            Variable objectVar= new Variable(
                    new VariableNameDecider(enhancedFor.getBody(), insertionPoint).suggest("obj"), b); //$NON-NLS-1$
            r.set(enhancedFor, EnhancedForStatement.PARAMETER_PROPERTY, b.declareSingleVariable(objectVar.varNameRaw(), objectType));

            // for (Map.Entry<K, V> mapEntry : map.entrySet()) {
            // Map.Entry mapEntry = (Map.Entry) obj; // <--- add this statement
            // Object key = mapEntry.getKey(); // <--- add this statement

            Type mapKeyType= b.createCopyTarget(parameter.getType());
            VariableDeclarationStatement newKeyDecl= b.declareStatement(mapKeyType, b.createMoveTarget(parameter.getName()),
                    b.invoke(entryVar.varName(), "getKey")); //$NON-NLS-1$

            r.insertFirst(enhancedFor.getBody(), Block.STATEMENTS_PROPERTY, newKeyDecl);

            if (keyUses > getValueMis.size()) {
                String mapEntryTypeName= typeNameDecider.useSimplestPossibleName(Entry.class.getCanonicalName());

                VariableDeclarationStatement newEntryDecl= b.declareStatement(b.type(mapEntryTypeName),
                        entryVar.varName(), b.cast(b.type(mapEntryTypeName), objectVar.varName()));
                r.insertFirst(enhancedFor.getBody(), Block.STATEMENTS_PROPERTY, newEntryDecl);
            }
        } else {
            // for (K key : map.keySet()) => for (K key : map.entrySet())
            r.set(enhancedFor, EnhancedForStatement.EXPRESSION_PROPERTY, b.invoke(b.createMoveTarget(mapExpression), "entrySet")); //$NON-NLS-1$
            // for (K key : map.entrySet()) => for (Map.Entry<K, V> mapEntry :
            // map.entrySet())
            Type mapEntryType= createMapEntryType(parameter, getValueMi0, typeNameDecider);
            r.set(enhancedFor, EnhancedForStatement.PARAMETER_PROPERTY, b.declareSingleVariable(entryVar.varNameRaw(), mapEntryType));

            if (keyUses > getValueMis.size()) {
                // for (Map.Entry<K, V> mapEntry : map.entrySet()) {
                // K key = mapEntry.getKey(); // <--- add this statement
                Type mapKeyType= b.createCopyTarget(parameter.getType());

                VariableDeclarationStatement newKeyDeclaration= b.declareStatement(mapKeyType,
                        b.createMoveTarget(parameter.getName()), b.invoke(entryVar.varName(), "getKey")); //$NON-NLS-1$
                r.insertFirst(enhancedFor.getBody(), Block.STATEMENTS_PROPERTY, newKeyDeclaration);
            }
        }

        // Replace all occurrences of map.get(key) => mapEntry.getValue()
        for (MethodInvocation getValueMi : getValueMis) {
            r.replace(getValueMi, b.invoke(entryVar.varName(), "getValue")); //$NON-NLS-1$
        }
    }

    /**
     * If possible, use the type declaration, so we can return the type as it was
     * declared. Otherwise, let's use the type binding and output verbose fully
     * qualified types.
     */
    private Type createMapEntryType(final SingleVariableDeclaration parameter, final MethodInvocation getValueMi,
            final TypeNameDecider typeNameDecider) {
        String mapEntryType= typeNameDecider.useSimplestPossibleName(Entry.class.getCanonicalName());

        ASTNodeFactory b= cuRewrite.getASTBuilder();
        Type paramType= parameter.getType();
        Type mapKeyType;
        if (paramType.isPrimitiveType()) {
            // Use the type binding (not as precise as what is in the code)
            ITypeBinding mapTypeBinding= getValueMi.getExpression().resolveTypeBinding();
            ITypeBinding keyTypeBinding= mapTypeBinding.getTypeArguments()[0];
            mapKeyType= b.toType(keyTypeBinding, typeNameDecider);
        } else {
            // Use the type as defined in the code
            mapKeyType= b.createMoveTarget(paramType);
        }
        Type mapValueType= b.copyType(getValueMi, typeNameDecider);
        return b.genericType(mapEntryType, mapKeyType, mapValueType);
    }

    private boolean isKeySetMethod(final MethodInvocation foreachExpression) {
        return foreachExpression != null && ASTNodes.usesGivenSignature(foreachExpression, Map.class.getCanonicalName(), "keySet"); //$NON-NLS-1$
    }

    private List<MethodInvocation> collectMapGetValueCalls(final Expression mapExpression,
            final SingleVariableDeclaration parameter, final Statement body) {
        return new CollectMapGetCalls(mapExpression, parameter).collect(body);
    }

    /** Sanity check. */
    private boolean haveSameTypeBindings(final Collection<? extends Expression> exprs) {
        Iterator<? extends Expression> it= exprs.iterator();

        if (!it.hasNext()) {
            // Not really expected
            return false;
        }

        ITypeBinding type0= it.next().resolveTypeBinding();

        if (type0 == null) {
            return false;
        }

        while (it.hasNext()) {
            ITypeBinding typeN= it.next().resolveTypeBinding();
            if (!areSameTypeBindings(type0, typeN)) {
                return false;
            }
        }

        return true;
    }

    private boolean areSameTypeBindings(final ITypeBinding type1, final ITypeBinding type2) {
        return type1 == null || type2 == null || (type1.isParameterizedType() == type2.isParameterizedType()
                && areSameParameterizedTypeBindings(type1, type2));
    }

    /** Special handling because of captures. */
    private boolean areSameParameterizedTypeBindings(final ITypeBinding type1, final ITypeBinding type2) {
        return type1.getErasure().equals(type2.getErasure())
                && areSameTypeBindings(type1.getTypeArguments(), type2.getTypeArguments());
    }

    private boolean areSameTypeBindings(final ITypeBinding[] types1, final ITypeBinding[] types2) {
        if (types1.length != types2.length) {
            return false;
        }
        for (int i= 0; i < types1.length; i++) {
            if (!areSameTypeBindings(types1[i], types2[i])) {
                return false;
            }
        }

        return true;
    }

    /**
     * Class to find {@code map.get(loopVariable)} constructs in the AST tree, and
     * collect the type of the value, which is unknown until one is located.
     */
    static class CollectMapGetCalls extends CollectorVisitor<MethodInvocation> {
        private final Expression mapExpression;
        private final SingleVariableDeclaration forEachParameter;

        public CollectMapGetCalls(final Expression mapExpression, final SingleVariableDeclaration forEachParameter) {
            this.mapExpression= mapExpression;
            this.forEachParameter= forEachParameter;
        }

        @Override
        public boolean visit(final MethodInvocation node) {
            if (isSameReference(node.getExpression(), mapExpression)
                    && ASTNodes.usesGivenSignature(node, Map.class.getCanonicalName(), "get", Object.class.getCanonicalName()) //$NON-NLS-1$
                    && ASTNodes.isSameVariable(ASTNodes.arguments(node).get(0), forEachParameter.getName())) {
                addResult(node);
            }

            return true;
        }

        private boolean isSameReference(final Expression expr1, final Expression expr2) {
            if (expr1 == null || expr2 == null) {
                return false;
            }
            if (expr1.getNodeType() != ASTNode.METHOD_INVOCATION || expr2.getNodeType() != ASTNode.METHOD_INVOCATION) {
                return ASTNodes.isSameVariable(expr1, expr2);
            }
            MethodInvocation mi1= (MethodInvocation) expr1;
            MethodInvocation mi2= (MethodInvocation) expr2;
            return ASTNodes.areBindingsEqual(mi1.resolveTypeBinding(), mi2.resolveTypeBinding())
                    && isSameReference(mi1.getExpression(), mi2.getExpression());
        }
    }
}
