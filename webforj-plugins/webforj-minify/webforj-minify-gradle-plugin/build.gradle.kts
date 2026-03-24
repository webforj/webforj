import javax.xml.parsers.DocumentBuilderFactory

plugins {
    `java-gradle-plugin`
    `maven-publish`
    id("com.gradle.plugin-publish") version "1.3.0" apply false
}

// Read version and Java release from root pom.xml dynamically
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
    // webforJ minify foundation module
    implementation("com.webforj:webforj-minify-foundation:$version")

    // Testing
    testImplementation("org.junit.jupiter:junit-jupiter:5.10.3")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher:1.10.3")
}

gradlePlugin {
    plugins {
        create("webforjMinify") {
            id = "com.webforj.minify"
            implementationClass = "com.webforj.minify.gradle.MinifyPlugin"
            displayName = "webforJ Minify Plugin"
            description = "Build-time asset minification for webforJ applications"
        }
    }
}

tasks.test {
    useJUnitPlatform()
}

tasks.withType<JavaCompile> {
    options.encoding = "UTF-8"
    options.release.set(javaRelease)
}

tasks.withType<Javadoc> {
    options.encoding = "UTF-8"
}

publishing {
    publications {
        create<MavenPublication>("maven") {
            from(components["java"])
        }
    }
}
