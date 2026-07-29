package com.webforj.devtools.craftforj.inspector.source.strategy;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.model.ModificationContext;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import com.webforj.devtools.craftforj.inspector.source.parser.AstFinder;
import com.webforj.devtools.craftforj.inspector.source.parser.AstModifier;
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

    String actualVarName = AstFinder.extractVariableName(field.get());
    if (actualVarName == null) {
      return;
    }

    String variableName = context.getVariableName();
    if (variableName != null && !variableName.isEmpty() && !variableName.equals(actualVarName)) {
      throw new SourceModificationException(
          "Variable mismatch at line " + context.getLineNumber() + ": expected '" + variableName
              + "' but found '" + actualVarName + "'. The source code may have changed.");
    }

    Optional<ClassOrInterfaceDeclaration> classDecl =
        cu.findFirst(ClassOrInterfaceDeclaration.class);
    if (classDecl.isEmpty()) {
      return;
    }

    List<ConstructorDeclaration> constructors = classDecl.get().getConstructors();
    BlockStmt block;
    if (constructors.isEmpty()) {
      ConstructorDeclaration ctor = classDecl.get().addConstructor();
      ctor.setBody(new BlockStmt());
      block = ctor.getBody();
    } else {
      block = constructors.get(0).getBody();
    }

    AstModifier.addSettersForVariable(cu, block, actualVarName, context.getSourceChanges());
  }
}
