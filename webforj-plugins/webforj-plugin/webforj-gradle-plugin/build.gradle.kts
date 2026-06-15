import javax.xml.parsers.DocumentBuilderFactory

plugins {
  `java-gradle-plugin`
  `maven-publish`
}

// Read the version and the Java release from the root pom.xml so the Gradle build stays aligned
// with the Maven reactor.
val rootPomFile = file("../../../pom.xml")
if (!rootPomFile.exists()) {
  throw GradleException("Root POM not found at ${rootPomFile.absolutePath}")
}

val rootDoc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(rootPomFile)

val parentVersion = rootDoc.getElementsByTagName("version").let { nodes ->
  if (nodes.length > 0) nodes.item(0).textContent
  else throw GradleException("Could not find version in root POM")
}

val javaRelease = rootDoc.getElementsByTagName("maven.compiler.release").let { nodes ->
  if (nodes.length > 0) nodes.item(0).textContent.toInt()
  else throw GradleException("Could not find maven.compiler.release in root POM")
}

group = "com.webforj"
version = parentVersion

java {
  toolchain {
    languageVersion.set(JavaLanguageVersion.of(javaRelease))
  }
  withSourcesJar()
  withJavadocJar()
}

repositories {
    mavenLocal()
    mavenCentral()
}

dependencies {
  implementation("com.webforj:webforj-plugin-foundation:$version")
  implementation("com.webforj:webforj-bundle-bun:$version")
  implementation("org.slf4j:slf4j-api:1.7.36")

  testImplementation("org.junit.jupiter:junit-jupiter:5.10.3")
  testRuntimeOnly("org.junit.platform:junit-platform-launcher:1.10.3")
  testImplementation("org.mockito:mockito-core:5.17.0")
  testImplementation("org.mockito:mockito-junit-jupiter:5.17.0")
}

gradlePlugin {
  plugins {
    create("webforj") {
      id = "com.webforj"
      implementationClass = "com.webforj.plugin.gradle.WebforjPlugin"
      displayName = "webforJ Plugin"
      description = "Build integration for webforJ applications"
    }
  }
}

tasks.test {
  useJUnitPlatform()
  testLogging {
    events("passed", "skipped", "failed")
    exceptionFormat = org.gradle.api.tasks.testing.logging.TestExceptionFormat.FULL
  }
}

tasks.withType<JavaCompile> {
  options.encoding = "UTF-8"
  options.release.set(javaRelease)
}

tasks.withType<Javadoc> {
  options.encoding = "UTF-8"
  // Gradle instantiates the managed task and extension types, so they cannot declare a documented
  // constructor. Drop the missing-comment doclint group, which here only flags those compiler
  // generated default constructors.
  (options as StandardJavadocDocletOptions).addBooleanOption("Xdoclint:all,-missing", true)
}

publishing {
  publications {
    create<MavenPublication>("maven") {
      from(components["java"])
    }
  }
}
