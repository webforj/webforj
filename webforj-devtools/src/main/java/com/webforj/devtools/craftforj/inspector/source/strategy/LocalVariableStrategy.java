package com.webforj.devtools.craftforj.inspector.source.strategy;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.model.ModificationContext;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import com.webforj.devtools.craftforj.inspector.source.parser.AstFinder;
import com.webforj.devtools.craftforj.inspector.source.parser.AstModifier;
import java.util.List;
import java.util.Optional;

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
  @SuppressWarnings("unchecked")
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

    Optional<Node> nodeAtLine = AstFinder.findNodeAt(cu, context.getTarget());
    BlockStmt block = null;

    if (nodeAtLine.isPresent()) {
      Optional<BlockStmt> parentBlock = nodeAtLine.get().findAncestor(BlockStmt.class);
      if (parentBlock.isPresent()) {
        block = parentBlock.get();
      }
    }

    if (block == null) {
      Optional<ClassOrInterfaceDeclaration> classDecl =
          cu.findFirst(ClassOrInterfaceDeclaration.class);
      if (classDecl.isPresent()) {
        List<ConstructorDeclaration> constructors = classDecl.get().getConstructors();
        if (constructors.isEmpty()) {
          ConstructorDeclaration ctor = classDecl.get().addConstructor();
          ctor.setBody(new BlockStmt());
          block = ctor.getBody();
        } else {
          block = constructors.get(0).getBody();
        }
      }
    }

    if (block != null) {
      AstModifier.addSettersForVariable(cu, block, actualVarName, context.getSourceChanges());
    }
  }
}
