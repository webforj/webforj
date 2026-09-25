package com.webforj.devtools.craftforj.source.strategy;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.ModificationContext;
import com.webforj.devtools.craftforj.source.model.TargetContext;
import com.webforj.devtools.craftforj.source.parser.AstFinder;
import com.webforj.devtools.craftforj.source.parser.AstModifier;
import java.util.List;

/**
 * Strategy for components declared as local variables.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class LocalVariableStrategy implements ModificationStrategy {

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean canHandle(CompilationUnit cu, TargetContext target) {
    if (AstFinder.findFieldAt(cu, target).isPresent()) {
      return false;
    }
    if (AstFinder.findInlineCreationAt(cu, target).isPresent()) {
      return false;
    }
    if (AstFinder.findFactoryMethodAt(cu, target).isPresent()) {
      return false;
    }
    return AstFinder.extractVariableNameAt(cu, target) != null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void apply(CompilationUnit cu, ModificationContext context) {
    String actualVarName = AstFinder.extractVariableNameAt(cu, context.getTarget());
    if (actualVarName == null) {
      return;
    }

    String variableName = context.getVariableName();
    if (variableName != null && !variableName.isEmpty() && !variableName.equals(actualVarName)) {
      throw new SourceModificationException(
          "Variable mismatch at line " + context.getLineNumber() + ": expected '" + variableName
              + "' but found '" + actualVarName + "'. The source code may have changed.");
    }

    BlockStmt block = findBlock(cu, context.getTarget());
    if (block == null) {
      return;
    }
    VariableDeclarator variable = AstFinder.findVariableAt(cu, context.getTarget()).orElse(null);
    if (variable != null) {
      AstModifier.addSettersForDeclaration(block, variable, context.getSourceChanges());
    } else {
      AstModifier.addSettersForVariable(block, actualVarName, context.getSourceChanges());
    }
  }

  @SuppressWarnings("unchecked")
  private static BlockStmt findBlock(CompilationUnit cu, TargetContext target) {
    BlockStmt enclosing = AstFinder.findNodeAt(cu, target)
        .flatMap(node -> node.findAncestor(BlockStmt.class)).orElse(null);
    if (enclosing != null) {
      return enclosing;
    }

    ClassOrInterfaceDeclaration classDecl =
        cu.findFirst(ClassOrInterfaceDeclaration.class).orElse(null);
    if (classDecl == null) {
      return null;
    }

    List<ConstructorDeclaration> constructors = classDecl.getConstructors();
    if (!constructors.isEmpty()) {
      return constructors.get(0).getBody();
    }

    ConstructorDeclaration ctor = classDecl.addConstructor();
    ctor.setBody(new BlockStmt());

    return ctor.getBody();
  }
}
