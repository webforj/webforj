package com.webforj.rewrite.v26;

import static org.openrewrite.java.Assertions.mavenProject;
import static org.openrewrite.java.Assertions.srcMainResources;
import static org.openrewrite.maven.Assertions.pomXml;
import static org.openrewrite.test.SourceSpecs.text;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RecipeSpec;
import org.openrewrite.test.RewriteTest;

@SuppressWarnings("java:S2699")
class MigrateInstallPluginSettingsTest implements RewriteTest {

  @Override
  public void defaults(RecipeSpec spec) {
    spec.recipe(new MigrateInstallPluginSettings());
  }

  @Test
  void movesCredentialsAndReplacesTheDeploymentPath() {
    rewriteRun(mavenProject("my-app", pomXml("""
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>proof</groupId>
          <artifactId>my-app</artifactId>
          <version>1.0-SNAPSHOT</version>
          <dependencies>
            <dependency>
              <groupId>com.webforj</groupId>
              <artifactId>webforj</artifactId>
              <version>26.02</version>
            </dependency>
          </dependencies>
          <build>
            <plugins>
              <plugin>
                <groupId>com.webforj</groupId>
                <artifactId>webforj-install-maven-plugin</artifactId>
                <version>26.02</version>
                <configuration>
                  <deployurl>https://bbj.example.test:8443/dwcj-install</deployurl>
                  <publishname>inventory</publishname>
                  <username>deployer</username>
                  <password>secret</password>
                  <token>abc</token>
                </configuration>
              </plugin>
            </plugins>
          </build>
        </project>
        """, """
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>proof</groupId>
          <artifactId>my-app</artifactId>
          <version>1.0-SNAPSHOT</version>
          <dependencies>
            <dependency>
              <groupId>com.webforj</groupId>
              <artifactId>webforj</artifactId>
              <version>26.02</version>
            </dependency>
            <dependency>
              <groupId>com.webforj</groupId>
              <artifactId>webforj-engine</artifactId>
              <version>26.02</version>
              <scope>provided</scope>
            </dependency>
          </dependencies>
          <build>
            <plugins>
              <plugin>
                <groupId>com.webforj</groupId>
                <artifactId>webforj-install-maven-plugin</artifactId>
                <version>26.02</version>
                <configuration>
                  <hotswap>
                    <hotswapAgent/>
                  </hotswap>
                  <bbj>
                    <publishName>inventory</publishName>
                    <deployment>
                      <url>https://bbj.example.test:8443/webforj-bbjship</url>
                    </deployment>
                    <admin>
                      <username>deployer</username>
                      <password>secret</password>
                      <token>abc</token>
                    </admin>
                  </bbj>
                </configuration>
              </plugin>
            </plugins>
          </build>
        </project>
        """), srcMainResources(text(null, """
        webforj.assetsSource = packaged
        webforj.devtools.livereload.enabled = true
        webforj.devtools.craftforj.enabled = true
        """, spec -> spec.path("webforj.conf")))));
  }

  @Test
  void addsTheEngineWithoutVersionWhenTheBomManagesIt() {
    rewriteRun(mavenProject("my-app", pomXml("""
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>proof</groupId>
          <artifactId>my-app</artifactId>
          <version>1.0-SNAPSHOT</version>
          <dependencyManagement>
            <dependencies>
              <dependency>
                <groupId>com.webforj</groupId>
                <artifactId>webforj</artifactId>
                <version>26.02</version>
              </dependency>
              <dependency>
                <groupId>com.webforj</groupId>
                <artifactId>webforj-engine</artifactId>
                <version>26.02</version>
              </dependency>
            </dependencies>
          </dependencyManagement>
          <dependencies>
            <dependency>
              <groupId>com.webforj</groupId>
              <artifactId>webforj</artifactId>
            </dependency>
          </dependencies>
          <build>
            <plugins>
              <plugin>
                <groupId>com.webforj</groupId>
                <artifactId>webforj-install-maven-plugin</artifactId>
                <version>26.02</version>
              </plugin>
            </plugins>
          </build>
        </project>
        """, """
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>proof</groupId>
          <artifactId>my-app</artifactId>
          <version>1.0-SNAPSHOT</version>
          <dependencyManagement>
            <dependencies>
              <dependency>
                <groupId>com.webforj</groupId>
                <artifactId>webforj</artifactId>
                <version>26.02</version>
              </dependency>
              <dependency>
                <groupId>com.webforj</groupId>
                <artifactId>webforj-engine</artifactId>
                <version>26.02</version>
              </dependency>
            </dependencies>
          </dependencyManagement>
          <dependencies>
            <dependency>
              <groupId>com.webforj</groupId>
              <artifactId>webforj</artifactId>
            </dependency>
            <dependency>
              <groupId>com.webforj</groupId>
              <artifactId>webforj-engine</artifactId>
              <scope>provided</scope>
            </dependency>
          </dependencies>
          <build>
            <plugins>
              <plugin>
                <groupId>com.webforj</groupId>
                <artifactId>webforj-install-maven-plugin</artifactId>
                <version>26.02</version>
                <configuration>
                  <hotswap>
                    <hotswapAgent/>
                  </hotswap>
                </configuration>
              </plugin>
            </plugins>
          </build>
        </project>
        """), srcMainResources(text("webforj.locale = \"de\"", """
        webforj.locale = "de"
        webforj.assetsSource = packaged
        webforj.devtools.livereload.enabled = true
        webforj.devtools.craftforj.enabled = true""", spec -> spec.path("webforj.conf")))));
  }

  @Test
  void mergesIntoAnExistingBuildPlugin() {
    rewriteRun(mavenProject("my-app", pomXml("""
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>proof</groupId>
          <artifactId>my-app</artifactId>
          <version>1.0-SNAPSHOT</version>
          <dependencies>
            <dependency>
              <groupId>com.webforj</groupId>
              <artifactId>webforj-engine</artifactId>
              <version>26.02</version>
              <scope>provided</scope>
            </dependency>
          </dependencies>
          <build>
            <plugins>
              <plugin>
                <groupId>com.webforj</groupId>
                <artifactId>webforj-maven-plugin</artifactId>
                <version>26.02</version>
                <configuration>
                  <eager>true</eager>
                </configuration>
              </plugin>
              <plugin>
                <groupId>com.webforj</groupId>
                <artifactId>webforj-install-maven-plugin</artifactId>
                <version>26.02</version>
                <configuration>
                  <publishname>inventory</publishname>
                </configuration>
              </plugin>
            </plugins>
          </build>
        </project>
        """, """
        <project>
          <modelVersion>4.0.0</modelVersion>
          <groupId>proof</groupId>
          <artifactId>my-app</artifactId>
          <version>1.0-SNAPSHOT</version>
          <dependencies>
            <dependency>
              <groupId>com.webforj</groupId>
              <artifactId>webforj-engine</artifactId>
              <version>26.02</version>
              <scope>provided</scope>
            </dependency>
          </dependencies>
          <build>
            <plugins>
              <plugin>
                <groupId>com.webforj</groupId>
                <artifactId>webforj-maven-plugin</artifactId>
                <version>26.02</version>
                <configuration>
                  <eager>true</eager>
                  <hotswap>
                    <hotswapAgent/>
                  </hotswap>
                  <bbj>
                    <publishName>inventory</publishName>
                  </bbj>
                </configuration>
              </plugin>
            </plugins>
          </build>
        </project>
        """), srcMainResources(text(null, """
        webforj.assetsSource = packaged
        webforj.devtools.livereload.enabled = true
        webforj.devtools.craftforj.enabled = true
        """, spec -> spec.path("webforj.conf")))));
  }
}
