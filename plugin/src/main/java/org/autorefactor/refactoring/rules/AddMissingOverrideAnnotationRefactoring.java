package org.autorefactor.refactoring.rules;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.MarkerAnnotation;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.internal.corext.util.MethodOverrideTester;
import org.eclipse.jdt.internal.corext.util.SuperTypeHierarchyCache;

@SuppressWarnings("restriction")
public class AddMissingOverrideAnnotationRefactoring extends AbstractRefactoringRule {

	private static final String OVERRIDE = "Override";

	@Override
	public String getDescription() {
		return "Add missing @Override annotation";
	}

	@Override
	public String getName() {
		return "Add missing @Override annotation";
	}

//	@Override
//    public boolean isEnabled(final Preferences prefs) {
//        return false; // prefs.addMissingOverrideAnnotations();
//    }
	
	public boolean visit(MethodDeclaration node) {
		try {
			@SuppressWarnings("unchecked")
			List<IExtendedModifier> modifiers = node.modifiers();
			if (-1 != findOverridenAnnotationModifier(modifiers)) {
				return true;
			}
			IMethod imeth = bindingToIMethod(node);
			if (imeth != null) {
				IType itype = imeth.getDeclaringType();
				MethodOverrideTester methodOverrideTester = SuperTypeHierarchyCache.getMethodOverrideTester(itype);
				IMethod overridenMeth = methodOverrideTester.findOverriddenMethod(imeth, true);
				if (overridenMeth != null) {
					// detected missing @Overriden
					final ASTBuilder b = ctx.getASTBuilder();
					Annotation overrideAnnotation = b.annotation(OVERRIDE);
					// modifiers.add(0, overrideAnnotation);
					ctx.getRefactorings().insertAt(node, MethodDeclaration.MODIFIERS2_PROPERTY, overrideAnnotation, 0);
				}
			}
		} catch(Exception ex) {
			// ignore!
		}
		return true;
	}
	
	public static int findOverridenAnnotationModifier(List<IExtendedModifier> modifiers) {
		for (int i = 0; i < modifiers.size(); i++) {
			IExtendedModifier m = modifiers.get(i);
			if (m instanceof MarkerAnnotation) {
				MarkerAnnotation ma = (MarkerAnnotation) m;
				String fqn = ma.getTypeName().getFullyQualifiedName();
				if (fqn.equals(OVERRIDE)) {
					return i;
				}
			}
		}
		return -1;
	}
	
	public static IMethod bindingToIMethod(MethodDeclaration node) {
		IMethodBinding binding = node.resolveBinding();
		if (binding != null) // Bindings can be null
			return (IMethod) binding.getJavaElement();
		return null;
	}
	
}
