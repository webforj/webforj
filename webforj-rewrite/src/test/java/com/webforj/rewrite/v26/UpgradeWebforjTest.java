package com.webforj.rewrite.v26;

import static org.assertj.core.api.Assertions.assertThat;
import static org.openrewrite.maven.Assertions.pomXml;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RecipeSpec;
import org.openrewrite.test.RewriteTest;

@SuppressWarnings("java:S2699")
class UpgradeWebforjTest implements RewriteTest {

  @Override
  public void defaults(RecipeSpec spec) {
    spec.recipeFromResources("com.webforj.rewrite.v26.UpgradeWebforj");
  }

  @Test
  void upgradesMavenCompilerReleaseTo21() {
    rewriteRun(pomXml("""
        <project>
            <modelVersion>4.0.0</modelVersion>
            <groupId>com.example</groupId>
            <artifactId>my-app</artifactId>
            <version>1.0-SNAPSHOT</version>
            <properties>
                <maven.compiler.release>17</maven.compiler.release>
            </properties>
        </project>
        """, """
        <project>
            <modelVersion>4.0.0</modelVersion>
            <groupId>com.example</groupId>
            <artifactId>my-app</artifactId>
            <version>1.0-SNAPSHOT</version>
            <properties>
                <maven.compiler.release>21</maven.compiler.release>
            </properties>
        </project>
        """));
  }

  @Test
  void upgradesWebforjVersionProperty() {
    rewriteRun(pomXml("""
        <project>
            <modelVersion>4.0.0</modelVersion>
            <groupId>com.example</groupId>
            <artifactId>my-app</artifactId>
            <version>1.0-SNAPSHOT</version>
            <properties>
                <webforj.version>25.12</webforj.version>
            </properties>
        </project>
        """, spec -> spec.after(actual -> assertThat(actual)
        .containsPattern("<webforj\\.version>26\\..*</webforj\\.version>").actual())));
  }

  @Test
  void doesNotDowngradeHigherJavaVersion() {
    rewriteRun(pomXml("""
        <project>
            <modelVersion>4.0.0</modelVersion>
            <groupId>com.example</groupId>
            <artifactId>my-app</artifactId>
            <version>1.0-SNAPSHOT</version>
            <properties>
                <maven.compiler.release>25</maven.compiler.release>
            </properties>
        </project>
        """));
  }
}
