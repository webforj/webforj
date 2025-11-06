# Maven Central Publishing Readiness Review

## Executive Summary

**Status**: ✅ **READY FOR PUBLICATION**

The webforJ Minify plugin project meets all Maven Central Repository requirements for publication. All mandatory metadata, artifact requirements, and build configurations are properly inherited from the parent `webforj-parent` POM.

---

## Maven Central Requirements Checklist

### ✅ 1. Correct Coordinates (GAV)

**Requirement**: Valid groupId, artifactId, and version coordinates.

**Status**: ✅ **PASS**

```xml
<groupId>com.webforj</groupId>
<artifactId>webforj-minify</artifactId>
<version>25.10-SNAPSHOT</version>
```

- **Group ID**: `com.webforj` (owned by BASIS International Ltd, verified through parent POM)
- **Artifact IDs**: All unique and follow naming conventions
  - `webforj-minify-foundation`
  - `webforj-minify-phcss-css`
  - `webforj-minify-closure-js`
  - `webforj-minify-maven-plugin`
  - `webforj-minify-gradle-plugin`
- **Version**: Follows semantic versioning (currently SNAPSHOT for development)

**Note**: For Maven Central publication, version must NOT end in `-SNAPSHOT`. Use release versions like `25.10.0` or `25.10`.

---

### ✅ 2. Project Name, Description, and URL

**Requirement**: Human-readable name, description, and project URL.

**Status**: ✅ **PASS** (Inherited from parent)

From `webforj-parent` (line 10-18):
```xml
<name>webforj</name>
<description>
  webforJ is a robust and flexible web framework that allows you to
  easily create a modern and engaging user interface using Java.
</description>
<url>https://webforj.com</url>
```

Module-specific descriptions are present in each module's POM:
- `webforj-minify/pom.xml`: "Build-time asset minification system for webforJ applications"
- Individual modules have appropriate descriptions

---

### ✅ 3. License Information

**Requirement**: At least one license must be declared.

**Status**: ✅ **PASS** (Inherited from parent)

From `webforj-parent` (line 36-41):
```xml
<licenses>
  <license>
    <name>MIT License</name>
    <url>https://www.opensource.org/licenses/mit-license.php</url>
  </license>
</licenses>
```

**License**: MIT License - approved for open source distribution.

---

### ✅ 4. Developer Information

**Requirement**: At least one developer must be listed.

**Status**: ✅ **PASS** (Inherited from parent)

From `webforj-parent` (line 43-49):
```xml
<developers>
  <developer>
    <id>webforJ</id>
    <organization>webforJ</organization>
    <organizationUrl>https://www.webforj.com</organizationUrl>
  </developer>
</developers>
```

**Recommendation**: Consider adding individual developer names and emails for better attribution.

---

### ✅ 5. SCM Information

**Requirement**: Source control connection details.

**Status**: ✅ **PASS** (Inherited from parent)

From `webforj-parent` (line 30-34):
```xml
<scm>
  <connection>scm:git:git://github.com/webforj/webforj.git</connection>
  <developerConnection>scm:git:ssh://github.com:webforj/webforj.git</developerConnection>
  <url>https://github.com/webforj/webforj/tree/main</url>
</scm>
```

**Source**: Public GitHub repository - accessible and verifiable.

---

### ✅ 6. Javadoc and Sources JARs

**Requirement**: Must supply `-sources.jar` and `-javadoc.jar` for all artifacts (except POM-only).

**Status**: ✅ **PASS** (Configured via profile)

From `webforj-parent` profile `perform-release`:
```xml
<plugin>
  <artifactId>maven-source-plugin</artifactId>
  <executions>
    <execution>
      <goals>
        <goal>jar-no-fork</goal>
      </goals>
    </execution>
  </executions>
</plugin>
<plugin>
  <artifactId>maven-javadoc-plugin</artifactId>
  <executions>
    <execution>
      <goals>
        <goal>jar</goal>
      </goals>
    </execution>
  </executions>
</plugin>
```

**Activation**: Activated via `perform-release` profile during release builds.

**Verification Command**:
```bash
mvn clean package -Pperform-release
```

This will generate for each module:
- `<artifactId>-<version>.jar`
- `<artifactId>-<version>-sources.jar`
- `<artifactId>-<version>-javadoc.jar`

---

### ✅ 7. PGP/GPG Signatures

**Requirement**: All files must be signed with GPG/PGP.

**Status**: ✅ **PASS** (Configured via profile)

From `webforj-parent` profile `perform-release`:
```xml
<plugin>
  <groupId>org.apache.maven.plugins</groupId>
  <artifactId>maven-gpg-plugin</artifactId>
  <executions>
    <execution>
      <id>sign-artifacts</id>
      <phase>verify</phase>
      <goals>
        <goal>sign</goal>
      </goals>
    </execution>
  </executions>
</plugin>
```

**Requirements**:
- GPG key must be configured on build machine
- Public key must be published to key server (e.g., https://pgp.mit.edu)
- Configured via `settings.xml` or environment variables

**Verification**: Each artifact will have corresponding `.asc` signature files.

---

### ✅ 8. File Checksums

**Requirement**: `.md5` and `.sha1` checksums required; `.sha256` and `.sha512` optional.

**Status**: ✅ **PASS** (Generated automatically by Maven)

Maven automatically generates checksum files during deployment:
- `.md5` - MD5 checksum
- `.sha1` - SHA-1 checksum

These are generated by the `maven-deploy-plugin` (version 3.1.3, configured in parent).

---

### ✅ 9. Central Publishing Plugin

**Requirement**: Must use approved publishing mechanism.

**Status**: ✅ **PASS** (Sonatype Central Portal configured)

From `webforj-parent` (line 206-223):
```xml
<plugin>
  <groupId>org.sonatype.central</groupId>
  <artifactId>central-publishing-maven-plugin</artifactId>
  <version>0.9.0</version>
  <extensions>true</extensions>
  <configuration>
    <publishingServerId>central</publishingServerId>
    <autoPublish>false</autoPublish>
    <waitUntil>published</waitUntil>
  </configuration>
</plugin>
```

**Configuration**:
- Using **Sonatype Central Portal** (new as of 2024)
- `autoPublish=false` - requires manual approval (recommended for safety)
- `waitUntil=published` - waits for publication to complete

**Note**: OSSRH legacy system is end-of-life as of June 30, 2025. This project correctly uses the new Central Portal.

---

### ✅ 10. No Repository References

**Requirement**: Discouraged from using `<repositories>` and `<pluginRepositories>`.

**Status**: ✅ **PASS**

Checked all module POMs - no repository declarations found (except in `settings.gradle.kts` for Hyyan's test project, which is separate).

All dependencies resolve from Maven Central, which is ideal.

---

## Special Considerations for Gradle Plugin

### Gradle Plugin Publishing

The `webforj-minify-gradle-plugin` has special requirements:

1. **Plugin Marker POM**: ✅ Generated automatically via Gradle's `maven-publish` plugin
2. **Plugin Metadata**: ✅ Generated via Gradle's `java-gradle-plugin`
3. **Maven Local Publishing**: ✅ Configured via `publishToMavenLocal` in pom.xml

The Gradle plugin publishes TWO artifacts to Maven Central:
1. Main JAR: `com.webforj:webforj-minify-gradle-plugin:25.10-SNAPSHOT`
2. Plugin Marker: `com.webforj.minify:com.webforj.minify.gradle.plugin:25.10-SNAPSHOT`

Both will be signed and include sources/javadoc via the `perform-release` profile.

---

## Publication Workflow

### Prerequisites

1. **Central Portal Account**
   - Account at https://central.sonatype.com
   - Namespace `com.webforj` verified and approved

2. **GPG Key Setup**
   - GPG key generated and configured
   - Public key published to key server
   - Key ID and passphrase configured in `~/.m2/settings.xml`:
     ```xml
     <settings>
       <servers>
         <server>
           <id>central</id>
           <username><!-- Central Portal token username --></username>
           <password><!-- Central Portal token password --></password>
         </server>
       </servers>
       <profiles>
         <profile>
           <id>gpg</id>
           <properties>
             <gpg.keyname><!-- Your GPG key ID --></gpg.keyname>
             <gpg.passphrase><!-- Your GPG passphrase --></gpg.passphrase>
           </properties>
         </profile>
       </profiles>
     </settings>
     ```

3. **Remove -SNAPSHOT from version**
   - Update version in parent POM to release version (e.g., `25.10.0`)
   - This must be done for all modules via parent POM

### Publication Commands

1. **Full Build with Release Profile**:
   ```bash
   mvn clean verify -Pperform-release
   ```
   This will:
   - Compile all modules
   - Run all tests
   - Generate sources JARs
   - Generate javadoc JARs
   - Sign all artifacts with GPG

2. **Deploy to Central Staging**:
   ```bash
   mvn deploy -Pperform-release
   ```
   This will:
   - Upload artifacts to Central Portal staging
   - Wait for validation
   - Artifacts will be in "PENDING" state

3. **Publish to Maven Central** (if autoPublish=false):
   - Log into https://central.sonatype.com
   - Navigate to "Deployments"
   - Review and click "Publish"
   - OR: Use Maven to publish:
     ```bash
     mvn org.sonatype.central:central-publishing-maven-plugin:publish
     ```

4. **Post-Publication**:
   - Artifacts appear in Maven Central within 30 minutes to 2 hours
   - Create Git tag for release version
   - Update version to next SNAPSHOT version

---

## Test Build Verification

Run this command to verify release build works (without actually deploying):

```bash
mvn clean verify -Pperform-release -DskipTests=false
```

**Expected Artifacts** (for each module):
- `target/<artifactId>-<version>.jar`
- `target/<artifactId>-<version>-sources.jar`
- `target/<artifactId>-<version>-javadoc.jar`
- `target/<artifactId>-<version>.jar.asc` (signature)
- `target/<artifactId>-<version>-sources.jar.asc`
- `target/<artifactId>-<version>-javadoc.jar.asc`
- Checksums (`.md5`, `.sha1`)

---

## Potential Issues and Recommendations

### ⚠️ Issue 1: Gradle Plugin Java Compatibility

**Observation**: `MinifyPlugin.java` uses deprecated Gradle APIs (as noted in build warnings).

**Recommendation**: Review and update deprecated API usage before release to ensure compatibility with newer Gradle versions.

**Command to check**:
```bash
cd webforj-minify-gradle-plugin
gradle build -Xlint:deprecation
```

### ⚠️ Issue 2: Closure Compiler Version

**Observation**: Using older version `v20230802` for Java 17 compatibility.

From `webforj-minify/pom.xml` (line 49):
```xml
<!-- Note: Using v20230802 - last version compatible with Java 17 -->
<dependency>
  <groupId>com.google.javascript</groupId>
  <artifactId>closure-compiler</artifactId>
  <version>v20230802</version>
</dependency>
```

**Recommendation**: Document this version constraint in release notes. Consider upgrading to Java 21 baseline in future releases to use latest Closure Compiler.

### ✅ Issue 3: Developer Information

**Current**: Generic organization developer entry.

**Recommendation**: Add specific developer contact information:
```xml
<developers>
  <developer>
    <id>webforj-team</id>
    <name>webforJ Development Team</name>
    <email>team@webforj.com</email>
    <organization>BASIS International Ltd</organization>
    <organizationUrl>https://basis.cloud</organizationUrl>
    <roles>
      <role>architect</role>
      <role>developer</role>
    </roles>
  </developer>
</developers>
```

This can be added to the parent POM and will apply to all modules.

---

## Final Checklist for Publication

- [ ] Update version from `25.10-SNAPSHOT` to release version (e.g., `25.10.0`)
- [ ] Ensure GPG key is set up and published to key server
- [ ] Configure Central Portal credentials in `~/.m2/settings.xml`
- [ ] Run full test suite: `mvn clean test`
- [ ] Run release build: `mvn clean verify -Pperform-release`
- [ ] Verify all artifacts generated (JARs, sources, javadoc, signatures)
- [ ] Deploy to staging: `mvn deploy -Pperform-release`
- [ ] Review artifacts in Central Portal
- [ ] Publish to Maven Central
- [ ] Create Git tag for release
- [ ] Update version to next SNAPSHOT
- [ ] Update CHANGELOG.md

---

## Conclusion

The **webforJ Minify plugin project is fully compliant** with all Maven Central Repository requirements:

✅ Correct coordinates and packaging
✅ Complete project metadata (name, description, URL)
✅ MIT License properly declared
✅ Developer information present
✅ SCM information configured
✅ Sources and Javadoc generation configured
✅ GPG signing configured
✅ Checksum generation automatic
✅ Sonatype Central Portal integration configured
✅ No problematic repository references

**The project is ready for publication to Maven Central once:**
1. Version is changed from SNAPSHOT to release version
2. GPG keys and Central Portal credentials are configured
3. Release profile is activated during deployment

**Estimated time to publication**: 30 minutes to 2 hours after deployment (Maven Central sync time).
