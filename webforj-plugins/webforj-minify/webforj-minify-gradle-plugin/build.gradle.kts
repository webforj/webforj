plugins {
    `java-gradle-plugin`
    `maven-publish`
    id("com.gradle.plugin-publish") version "1.3.0" apply false
}

// TODO: Read version from parent pom.xml dynamically
// For now, must match parent POM version
group = "com.webforj"
version = "25.10-SNAPSHOT"

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
    withSourcesJar()
    withJavadocJar()
}

repositories {
    mavenCentral()
    mavenLocal()
}

dependencies {
    // webforJ minify foundation module
    implementation("com.webforj:webforj-minify-foundation:25.10-SNAPSHOT")

    // Testing
    testImplementation("org.junit.jupiter:junit-jupiter:5.10.3")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
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

            pom {
                name.set("webforJ Minify Gradle Plugin")
                description.set("Gradle plugin for minifying webforJ assets at build time")
                url.set("https://github.com/webforj/webforj")

                licenses {
                    license {
                        name.set("MIT License")
                        url.set("https://opensource.org/licenses/MIT")
                    }
                }

                developers {
                    developer {
                        id.set("webforj")
                        name.set("webforJ Team")
                        email.set("info@webforj.com")
                    }
                }

                scm {
                    connection.set("scm:git:git://github.com/webforj/webforj.git")
                    developerConnection.set("scm:git:ssh://github.com/webforj/webforj.git")
                    url.set("https://github.com/webforj/webforj")
                }
            }
        }
    }
}
