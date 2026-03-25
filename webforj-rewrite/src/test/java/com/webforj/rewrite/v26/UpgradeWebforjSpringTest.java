package com.webforj.rewrite.v26;

import static org.openrewrite.maven.Assertions.pomXml;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RecipeSpec;
import org.openrewrite.test.RewriteTest;

@SuppressWarnings("java:S2699")
class UpgradeWebforjSpringTest implements RewriteTest {

  @Override
  public void defaults(RecipeSpec spec) {
    spec.recipeFromResources("com.webforj.rewrite.v26.UpgradeWebforjSpring");
  }

  @Test
  void removesTomcatVersionProperty() {
    rewriteRun(pomXml("""
        <project>
            <modelVersion>4.0.0</modelVersion>
            <groupId>com.example</groupId>
            <artifactId>my-app</artifactId>
            <version>1.0-SNAPSHOT</version>
            <properties>
                <maven.compiler.release>17</maven.compiler.release>
                <tomcat.version>10.1.39</tomcat.version>
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
  void removesWebforjDependency() {
    rewriteRun(pomXml("""
        <project>
            <modelVersion>4.0.0</modelVersion>
            <groupId>com.example</groupId>
            <artifactId>my-app</artifactId>
            <version>1.0-SNAPSHOT</version>
            <properties>
                <webforj.version>25.12</webforj.version>
            </properties>
            <dependencies>
                <dependency>
                    <groupId>com.webforj</groupId>
                    <artifactId>webforj</artifactId>
                    <version>${webforj.version}</version>
                </dependency>
                <dependency>
                    <groupId>com.webforj</groupId>
                    <artifactId>webforj-spring-boot-starter</artifactId>
                    <version>${webforj.version}</version>
                </dependency>
            </dependencies>
        </project>
        """, """
        <project>
            <modelVersion>4.0.0</modelVersion>
            <groupId>com.example</groupId>
            <artifactId>my-app</artifactId>
            <version>1.0-SNAPSHOT</version>
            <properties>
                <maven.compiler.release>21</maven.compiler.release>
                <webforj.version>26.00-SNAPSHOT</webforj.version>
            </properties>
            <dependencies>
                <dependency>
                    <groupId>com.webforj</groupId>
                    <artifactId>webforj-spring-boot-starter</artifactId>
                    <version>${webforj.version}</version>
                </dependency>
            </dependencies>
        </project>
        """));
  }
}
