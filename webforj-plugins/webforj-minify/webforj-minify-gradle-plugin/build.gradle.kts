import javax.xml.parsers.DocumentBuilderFactory

plugins {
    `java-gradle-plugin`
    `maven-publish`
    id("com.gradle.plugin-publish") version "1.3.0" apply false
}

// Read version from parent pom.xml dynamically
val parentPomFile = file("../pom.xml")
val parentVersion = if (parentPomFile.exists()) {
    val docBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder()
    val doc = docBuilder.parse(parentPomFile)
    val versionNodes = doc.getElementsByTagName("version")
    if (versionNodes.length > 0) {
        versionNodes.item(0).textContent
    } else {
        throw GradleException("Could not find version in parent POM")
    }
} else {
    throw GradleException("Parent POM not found at ${parentPomFile.absolutePath}")
}

group = "com.webforj"
version = parentVersion

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
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
    options.release.set(17)
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
