# webforj-rewrite

OpenRewrite recipes for automated migration between webforJ versions.

## How to upgrade

### 1. Add the plugin

Add the OpenRewrite Maven plugin to your `pom.xml`:

```xml
<plugin>
  <groupId>org.openrewrite.maven</groupId>
  <artifactId>rewrite-maven-plugin</artifactId>
  <version>LATEST</version>
  <configuration>
    <activeRecipes>
      <recipe>RECIPE_NAME</recipe>
    </activeRecipes>
  </configuration>
  <dependencies>
    <dependency>
      <groupId>com.webforj</groupId>
      <artifactId>webforj-rewrite</artifactId>
      <version>TARGET_VERSION</version>
    </dependency>
  </dependencies>
</plugin>
```

Replace `TARGET_VERSION` with the webforJ version you are upgrading to and `RECIPE_NAME` with the appropriate recipe from the table below.

### 2. Preview changes (optional)

```bash
mvn rewrite:dryRun
```

This generates a diff in `target/rewrite/rewrite.patch` without modifying any files. Review the patch to see exactly what the recipe will change.

### 3. Run the recipe

```bash
mvn rewrite:run
```

The recipe handles the majority of the upgrade automatically — updating dependencies, renaming methods, changing types, and adjusting return types. For the few cases where a 1:1 replacement doesn't exist, it adds `TODO webforJ` comments with specific instructions. Search your project for these TODOs to find what's left.

### 4. Clean up

After fixing all TODOs, remove the plugin from your `pom.xml`.

## Recipes

### v26

| Recipe | Use when |
|---|---|
| `com.webforj.rewrite.v26.UpgradeWebforj` | Standard webforJ projects |
| `com.webforj.rewrite.v26.UpgradeWebforjSpring` | Spring Boot projects |
