package com.webforj.devtools.craftforj.inspector.source.strategy;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import com.webforj.devtools.craftforj.inspector.source.model.ModificationContext;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class FactoryMethodStrategyTest {

  private FactoryMethodStrategy strategy;

  @BeforeEach
  void setUp() {
    strategy = new FactoryMethodStrategy();
  }

  @Test
  void shouldHandleStaticFactoryMethod() {
    String code = """
        class Test {
          void method() {
            add(Icon.create("test"));
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(3, "Icon"));

    assertTrue(canHandle);
  }

  @Test
  void shouldHandleEnumConstantFactoryMethod() {
    String code = """
        class Test {
          void method() {
            add(FeatherIcon.BELL.create());
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(3, "Icon"));

    assertTrue(canHandle);
  }

  @Test
  void shouldNotHandleInstanceMethodCall() {
    String code = """
        class Test {
          void method() {
            button.setText("Hello");
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(3, "Icon"));

    assertFalse(canHandle);
  }

  @Test
  void shouldNotHandleAssignedFactoryCall() {
    String code = """
        class Test {
          void method() {
            Icon icon = Icon.create("test");
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(3, "Icon"));

    assertFalse(canHandle);
  }

  @Test
  void shouldExtractToVariableAndAddSetter() {
    String code = """
        class Test {
          void method() {
            add(Icon.create("test"));
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setSize", new StringLiteralExpr("24")).build();
    TargetContext target = new TargetContext(3, "Icon");

    strategy.apply(cu, new ModificationContext(target, null, List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("Icon icon = Icon.create"));
    assertTrue(result.contains("icon.setSize"));
    assertTrue(result.contains("add(icon)"));
  }

  @Test
  void shouldExtractEnumFactoryToVariableAndAddSetter() {
    String code = """
        class Test {
          void method() {
            add(FeatherIcon.BELL.create());
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setSize", new StringLiteralExpr("24")).build();
    TargetContext target = new TargetContext(3, "Icon");

    strategy.apply(cu, new ModificationContext(target, null, List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("FeatherIcon.BELL.create()"));
    assertTrue(result.contains("icon.setSize"));
    assertTrue(result.contains("add(icon)"));
  }

}
