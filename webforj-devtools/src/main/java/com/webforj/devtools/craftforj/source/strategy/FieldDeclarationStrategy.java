package com.webforj.devtools.craftforj.source.strategy;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.BodyDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.InitializerDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.ModificationContext;
import com.webforj.devtools.craftforj.source.model.SourceChange;
import com.webforj.devtools.craftforj.source.model.TargetContext;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.parser.AstModifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Strategy for components declared as class fields.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class FieldDeclarationStrategy implements ModificationStrategy {

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean canHandle(CompilationUnit cu, TargetContext target) {
    return AstFinder.findFieldAt(cu, target).isPresent();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void apply(CompilationUnit cu, ModificationContext context) {
    Optional<FieldDeclaration> field = AstFinder.findFieldAt(cu, context.getTarget());
    if (field.isEmpty()) {
      return;
    }

    VariableDeclarator variable = AstFinder.findVariableAt(cu, context.getTarget()).orElseThrow();
    String actualVarName = variable.getNameAsString();

    String variableName = context.getVariableName();
    if (variableName != null && !variableName.isEmpty() && !variableName.equals(actualVarName)) {
      throw new SourceModificationException(
          "Variable mismatch at line " + context.getLineNumber() + ": expected '" + variableName
              + "' but found '" + actualVarName + "'. The source code may have changed.");
    }

    Node owner = field.get().getParentNode().orElse(null);
    NodeList<BodyDeclaration<?>> members;
    if (owner instanceof ObjectCreationExpr creation) {
      members = creation.getAnonymousClassBody().orElseThrow();
    } else if (owner instanceof ClassOrInterfaceDeclaration classDecl) {
      members = classDecl.getMembers();
    } else {
      throw new SourceModificationException("Cannot determine the owning class for field '"
          + actualVarName + "' at line " + context.getLineNumber());
    }

    List<BlockStmt> initializers = new ArrayList<>();
    List<ConstructorDeclaration> constructors = new ArrayList<>();
    for (BodyDeclaration<?> member : members) {
      if (member instanceof InitializerDeclaration initializer && !initializer.isStatic()) {
        initializers.add(initializer.getBody());
      } else if (member instanceof ConstructorDeclaration constructor) {
        constructors.add(constructor);
      }
    }

    for (SourceChange change : context.getSourceChanges()) {
      boolean found = AstModifier.updateInitializerSetter(variable, change);
      for (BlockStmt block : initializers) {
        if (AstModifier.hasSetterForDeclaration(block, variable, change)) {
          AstModifier.addSettersForDeclaration(cu, block, variable, List.of(change));
          found = true;
        }
      }
      for (ConstructorDeclaration constructor : constructors) {
        BlockStmt block = constructor.getBody();
        if (AstModifier.hasSetterForDeclaration(block, variable, change)
            || !found && !change.isRemoval() && !AstFinder.isDelegatingConstructor(constructor)) {
          AstModifier.addSettersForDeclaration(cu, block, variable, List.of(change));
        }
      }
      if (constructors.isEmpty() && !found && !change.isRemoval()) {
        BlockStmt block =
            owner instanceof ClassOrInterfaceDeclaration classDecl ? getConstructorBody(classDecl)
                : getAnonymousInitializer((ObjectCreationExpr) owner, field.get());
        AstModifier.addSettersForDeclaration(cu, block, variable, List.of(change));
      }
    }
  }

  private BlockStmt getConstructorBody(ClassOrInterfaceDeclaration classDecl) {
    if (!classDecl.getConstructors().isEmpty()) {
      return classDecl.getConstructors().get(0).getBody();
    }
    ConstructorDeclaration constructor = classDecl.addConstructor();
    constructor.setBody(new BlockStmt());
    return constructor.getBody();
  }

  private BlockStmt getAnonymousInitializer(ObjectCreationExpr creation, FieldDeclaration field) {
    NodeList<BodyDeclaration<?>> members = creation.getAnonymousClassBody().orElseThrow();
    int fieldIndex = members.indexOf(field);
    for (int index = fieldIndex + 1; index < members.size(); index++) {
      if (members.get(index) instanceof InitializerDeclaration initializer
          && !initializer.isStatic()) {
        return initializer.getBody();
      }
    }

    InitializerDeclaration initializer = new InitializerDeclaration(false, new BlockStmt());
    members.add(fieldIndex + 1, initializer);
    return initializer.getBody();
  }
}
