package org.autorefactor.refactoring.rules;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Comparator;

import org.autorefactor.preferences.Preferences;
import org.autorefactor.refactoring.JavaRefactoringRule;
import org.autorefactor.refactoring.RefactoringRule;
import org.autorefactor.refactoring.Refactorings;
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
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.EnumConstantDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
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

/** Forwards method calls to a delegate ASTVisitor. */
public class ForwardingASTVisitor extends ASTVisitor implements JavaRefactoringRule {

    @Override
    public String getDescription() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getName() {
        throw new UnsupportedOperationException();
    }

    private final ASTVisitor delegate;
    private final boolean isRefactoringRule;
    private RefactoringContext ctx;

    /**
     * Constructor.
     *
     * @param delegate the delegate ASTVisitor where method calls will be forwarded to.
     */
    public ForwardingASTVisitor(ASTVisitor delegate) {
        this.delegate = delegate;
        this.isRefactoringRule = delegate instanceof RefactoringRule;
    }

    @Override
    public boolean isEnabled(Preferences preferences) {
        if (isRefactoringRule) {
            return ((RefactoringRule) delegate).isEnabled(preferences);
        }
        return true;
    }

    @Override
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
        if (isRefactoringRule) {
            ((RefactoringRule) delegate).setRefactoringContext(ctx);
        }
    }

    @Override
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }

    @Override
    public void endVisit(ExpressionStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(FieldAccess node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(EnumDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(FieldDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ForStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(IfStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ContinueStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(DoStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(EmptyStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(EnhancedForStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(EnumConstantDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(LabeledStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(LineComment node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(MarkerAnnotation node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(MemberRef node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(MemberValuePair node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ImportDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(InfixExpression node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(InstanceofExpression node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(Initializer node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(Javadoc node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ArrayCreation node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ArrayInitializer node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ArrayType node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(AssertStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(Assignment node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(Block node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(WildcardType node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(AnnotationTypeDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(AnnotationTypeMemberDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(AnonymousClassDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ArrayAccess node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(CharacterLiteral node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ClassInstanceCreation node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(CompilationUnit node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ConditionalExpression node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ConstructorInvocation node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(BlockComment node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(BooleanLiteral node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(BreakStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(CastExpression node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(CatchClause node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(SwitchStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(SynchronizedStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(TagElement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(TextElement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ThisExpression node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ThrowStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(StringLiteral node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(SuperConstructorInvocation node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(SuperFieldAccess node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(SuperMethodInvocation node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(SwitchCase node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(UnionType node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(VariableDeclarationExpression node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(VariableDeclarationStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(VariableDeclarationFragment node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(WhileStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(TryStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(TypeDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(TypeDeclarationStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(TypeLiteral node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(TypeParameter node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(NormalAnnotation node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(NullLiteral node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(NumberLiteral node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(PackageDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ParameterizedType node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ParenthesizedExpression node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(MethodRef node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(MethodRefParameter node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(MethodDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(MethodInvocation node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(Modifier node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(ReturnStatement node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(SimpleName node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(SimpleType node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(SingleMemberAnnotation node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(SingleVariableDeclaration node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(PostfixExpression node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(PrefixExpression node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(PrimitiveType node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(QualifiedName node) {
        delegate.endVisit(node);
    }

    @Override
    public void endVisit(QualifiedType node) {
        delegate.endVisit(node);
    }

    @Override
    public void postVisit(ASTNode node) {
        delegate.postVisit(node);
    }

    @Override
    public void preVisit(ASTNode node) {
        delegate.preVisit(node);
    }

    @Override
    public boolean preVisit2(ASTNode node) {
        return delegate.preVisit2(node);
    }

    @Override
    public boolean visit(ExpressionStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(EnumDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(EnumConstantDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(FieldAccess node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(FieldDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ForStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ConstructorInvocation node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ContinueStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(DoStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(EmptyStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(EnhancedForStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(Javadoc node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(LabeledStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(LineComment node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(MarkerAnnotation node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(MemberRef node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(IfStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ImportDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(InfixExpression node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(InstanceofExpression node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(Initializer node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ArrayCreation node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ArrayInitializer node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ArrayType node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(AssertStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(Assignment node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(VariableDeclarationFragment node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(AnnotationTypeDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(AnnotationTypeMemberDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(AnonymousClassDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ArrayAccess node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(CatchClause node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(CharacterLiteral node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ClassInstanceCreation node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(CompilationUnit node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ConditionalExpression node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(Block node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(BlockComment node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(BooleanLiteral node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(BreakStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(CastExpression node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(SwitchCase node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(SwitchStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(SynchronizedStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(TagElement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(TextElement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ThisExpression node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(SingleVariableDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(StringLiteral node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(SuperConstructorInvocation node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(SuperFieldAccess node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(SuperMethodInvocation node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(TypeParameter node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(UnionType node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(VariableDeclarationExpression node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(WildcardType node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(WhileStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(VariableDeclarationStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ThrowStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(TryStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(TypeDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(TypeDeclarationStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(TypeLiteral node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(Modifier node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(NormalAnnotation node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(NullLiteral node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(NumberLiteral node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(PackageDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ParameterizedType node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(MemberValuePair node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(MethodRef node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(MethodRefParameter node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(MethodDeclaration node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(MethodInvocation node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(QualifiedType node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ReturnStatement node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(SimpleName node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(SimpleType node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(SingleMemberAnnotation node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(ParenthesizedExpression node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(PostfixExpression node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(PrefixExpression node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(PrimitiveType node) {
        return delegate.visit(node);
    }

    @Override
    public boolean visit(QualifiedName node) {
        return delegate.visit(node);
    }

    /**
     * Generates the code for all the ASTVisitor methods that delegate to the
     * underlying visitors.
     *
     * @param args
     *            the arguments of the Java program
     */
    public static void main(String[] args) {
        final Method[] mm = ASTVisitor.class.getDeclaredMethods();
        Arrays.sort(mm, new Comparator<Method>() {
            @Override
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
            if (Void.TYPE.equals(m.getReturnType())) {
                System.out.print("\t");
            } else {
                System.out.print("\treturn ");
            }
            System.out.println("delegate." + m.getName() + "(node);");
            System.out.println("}");
            System.out.println();
        }
    }
}
