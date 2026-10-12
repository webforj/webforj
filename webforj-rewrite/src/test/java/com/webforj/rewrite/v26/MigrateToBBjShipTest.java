package com.webforj.rewrite.v26;

import static org.assertj.core.api.Assertions.assertThat;
import static org.openrewrite.java.Assertions.mavenProject;
import static org.openrewrite.java.Assertions.srcMainResources;
import static org.openrewrite.maven.Assertions.pomXml;
import static org.openrewrite.test.SourceSpecs.text;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RecipeSpec;
import org.openrewrite.test.RewriteTest;

@SuppressWarnings("java:S2699")
class MigrateToBBjShipTest implements RewriteTest {

  private static final String INSTALL_POM =
      """
          <?xml version="1.0" encoding="UTF-8"?>
          <project xmlns="http://maven.apache.org/POM/4.0.0"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
            <modelVersion>4.0.0</modelVersion>

            <groupId>proof</groupId>
            <artifactId>my-app</artifactId>
            <name>MyApp</name>
            <version>1.0-SNAPSHOT</version>

            <properties>
              <webforj.version>26.02</webforj.version>
            </properties>

            <dependencies>
              <dependency>
                <groupId>com.webforj</groupId>
                <artifactId>webforj</artifactId>
                <version>${webforj.version}</version>
                <exclusions>
                  <exclusion>
                    <groupId>com.webforj</groupId>
                    <artifactId>webforj-engine</artifactId>
                  </exclusion>
                </exclusions>
              </dependency>
            </dependencies>

            <build>
              <plugins>
                <plugin>
                  <groupId>com.webforj</groupId>
                  <artifactId>webforj-install-maven-plugin</artifactId>
                  <version>${webforj.version}</version>
                  <executions>
                    <execution>
                      <goals>
                        <goal>install</goal>
                      </goals>
                    </execution>
                  </executions>
                  <configuration>
                    <deployurl>http://localhost:8888/webforj-install</deployurl>
                    <classname>proof.Application</classname>
                    <publishname>hello-world</publishname>
                    <debug>true</debug>
                  </configuration>
                </plugin>
              </plugins>
            </build>
          </project>
          """;

  private static final String BBJSHIP_POM =
      """
          <?xml version="1.0" encoding="UTF-8"?>
          <project xmlns="http://maven.apache.org/POM/4.0.0"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
            <modelVersion>4.0.0</modelVersion>

            <groupId>proof</groupId>
            <artifactId>my-app</artifactId>
            <name>MyApp</name>
            <version>1.0-SNAPSHOT</version>

            <properties>
              <webforj.version>26.02</webforj.version>
            </properties>

            <dependencies>
              <dependency>
                <groupId>com.webforj</groupId>
                <artifactId>webforj</artifactId>
                <version>${webforj.version}</version>
                <exclusions>
                  <exclusion>
                    <groupId>com.webforj</groupId>
                    <artifactId>webforj-engine</artifactId>
                  </exclusion>
                </exclusions>
              </dependency>
              <dependency>
                <groupId>com.webforj</groupId>
                <artifactId>webforj-engine</artifactId>
                <version>${webforj.version}</version>
                <scope>provided</scope>
              </dependency>
            </dependencies>

            <build>
              <plugins>
                <plugin>
                  <groupId>com.webforj</groupId>
                  <artifactId>webforj-maven-plugin</artifactId>
                  <version>${webforj.version}</version>
                  <configuration>
                    <hotswap>
                      <hotswapAgent/>
                    </hotswap>
                    <bbj>
                      <publishName>hello-world</publishName>
                      <deployment>
                        <url>http://localhost:8888/webforj-bbjship</url>
                      </deployment>
                    </bbj>
                  </configuration>
                  <extensions>true</extensions>
                </plugin>
              </plugins>
              <defaultGoal>compile webforj:watch</defaultGoal>
            </build>
          </project>
          """;

  @Override
  public void defaults(RecipeSpec spec) {
    spec.recipeFromResources("com.webforj.rewrite.v26.MigrateToBBjShip");
  }

  @Test
  void migratesInstallPluginProjectToBBjShip() {
    rewriteRun(
        mavenProject("my-app", pomXml(INSTALL_POM, BBJSHIP_POM), srcMainResources(text(null, """
            webforj.entry = "proof.Application"
            webforj.debug = true
            webforj.assetsSource = packaged
            webforj.devtools.livereload.enabled = true
            webforj.devtools.craftforj.enabled = true
            """, spec -> spec.path("webforj.conf")))));
  }

  @Test
  void keepsExistingConfigurationKeys() {
    rewriteRun(mavenProject("my-app", pomXml(INSTALL_POM, BBJSHIP_POM), srcMainResources(text("""
        webforj.debug = false
        webforj.locale = "de"
        """, """
        webforj.debug = false
        webforj.locale = "de"
        webforj.entry = "proof.Application"
        webforj.assetsSource = packaged
        webforj.devtools.livereload.enabled = true
        webforj.devtools.craftforj.enabled = true
        """, spec -> spec.path("webforj.conf")))));
  }

  @Test
  void leavesBBjShipProjectUnchanged() {
    rewriteRun(mavenProject("my-app", pomXml(BBJSHIP_POM)));
  }

  @Test
  void leavesProjectWithoutInstallPluginUnchanged() {
    rewriteRun(mavenProject("my-app", pomXml("""
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>proof</groupId>
          <artifactId>my-app</artifactId>
          <version>1.0-SNAPSHOT</version>
        </project>
        """)));
  }

  @Test
  void upgradeTo2603MigratesInstallPluginProject() {
    rewriteRun(spec -> spec.recipeFromResources("com.webforj.rewrite.v26.UpgradeWebforj_26_03"),
        mavenProject("my-app",
            pomXml(INSTALL_POM,
                spec -> spec.after(actual -> assertThat(actual)
                    .contains("<artifactId>webforj-maven-plugin</artifactId>")
                    .doesNotContain("webforj-install-maven-plugin").actual())),
            srcMainResources(text(null, """
                webforj.entry = "proof.Application"
                webforj.debug = true
                webforj.assetsSource = packaged
                webforj.devtools.livereload.enabled = true
                webforj.devtools.craftforj.enabled = true
                """, spec -> spec.path("webforj.conf")))));
  }
}
