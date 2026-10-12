package com.webforj.rewrite.v26;

import java.net.URI;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import org.openrewrite.ExecutionContext;
import org.openrewrite.ScanningRecipe;
import org.openrewrite.SourceFile;
import org.openrewrite.Tree;
import org.openrewrite.TreeVisitor;
import org.openrewrite.text.PlainText;
import org.openrewrite.xml.AddToTagVisitor;
import org.openrewrite.xml.RemoveContentVisitor;
import org.openrewrite.xml.XmlIsoVisitor;
import org.openrewrite.xml.tree.Xml;

/**
 * Moves the settings of the webforJ install plugin of a project to their BBjShip places.
 *
 * <p>
 * The deployment URL and the publish name move to the {@code bbj} block of the plugin, with the
 * development run settings beside them. The entry class and the debug flag move to
 * {@code webforj.conf}, together with the settings a BBjShip app runs with.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class MigrateInstallPluginSettings
    extends ScanningRecipe<MigrateInstallPluginSettings.Accumulator> {

  private static final String INSTALL_PLUGIN = "webforj-install-maven-plugin";
  private static final String BUILD_PLUGIN = "webforj-maven-plugin";
  private static final String DEPLOYMENT_PATH = "/webforj-bbjship";
  private static final String POM = "pom.xml";
  private static final String ARTIFACT_ID = "artifactId";
  private static final String PLUGIN = "plugin";
  private static final String CONFIGURATION_TAG = "configuration";
  private static final Path CONFIGURATION = Path.of("src/main/resources/webforj.conf");
  private static final String ENGINE_DEPENDENCY = """
      <dependency>
        <groupId>com.webforj</groupId>
        <artifactId>webforj-engine</artifactId>
        %s<scope>provided</scope>
      </dependency>""";

  @Override
  public String getDisplayName() {
    return "Move the webforJ install plugin settings to BBjShip";
  }

  @Override
  public String getDescription() {
    return "Moves the deployment URL and the publish name of the webforJ install plugin to the "
        + "bbj block, and the entry class and the debug flag to webforj.conf.";
  }

  @Override
  public Accumulator getInitialValue(ExecutionContext ctx) {
    return new Accumulator();
  }

  @Override
  public TreeVisitor<?, ExecutionContext> getScanner(Accumulator acc) {
    return new TreeVisitor<Tree, ExecutionContext>() {

      @Override
      public Tree visit(Tree tree, ExecutionContext ctx) {
        if (tree instanceof SourceFile source) {
          acc.files.add(source.getSourcePath());
        }

        if (tree instanceof Xml.Document document && isPom(document)) {
          findInstallSettings(document)
              .ifPresent(settings -> acc.settings.put(getModule(document), settings));
        }

        return tree;
      }
    };
  }

  @Override
  public Collection<? extends SourceFile> generate(Accumulator acc, ExecutionContext ctx) {
    List<SourceFile> generated = new ArrayList<>();
    acc.settings.forEach((module, settings) -> {
      Path configuration = module.resolve(CONFIGURATION);
      if (!acc.files.contains(configuration)) {
        generated.add(PlainText.builder().sourcePath(configuration)
            .text(formatConfiguration("", settings)).build());
      }
    });

    return generated;
  }

  @Override
  public TreeVisitor<?, ExecutionContext> getVisitor(Accumulator acc) {
    return new TreeVisitor<Tree, ExecutionContext>() {

      @Override
      public Tree visit(Tree tree, ExecutionContext ctx) {
        if (tree instanceof Xml.Document document && isPom(document)) {
          InstallSettings settings = acc.settings.get(getModule(document));

          return settings == null ? tree : new PomMigration(settings).visitNonNull(document, ctx);
        }

        if (tree instanceof PlainText text) {
          return migrateConfiguration(text, acc);
        }

        return tree;
      }
    };
  }

  private static PlainText migrateConfiguration(PlainText text, Accumulator acc) {
    for (Map.Entry<Path, InstallSettings> entry : acc.settings.entrySet()) {
      if (text.getSourcePath().equals(entry.getKey().resolve(CONFIGURATION))) {
        String content = formatConfiguration(text.getText(), entry.getValue());

        return content.equals(text.getText()) ? text : text.withText(content);
      }
    }

    return text;
  }

  private static String formatConfiguration(String text, InstallSettings settings) {
    Map<String, String> keys = new LinkedHashMap<>();
    if (settings.classname != null) {
      keys.put("webforj.entry", "\"" + settings.classname + "\"");
    }

    if (settings.debug != null) {
      keys.put("webforj.debug", settings.debug);
    }

    keys.put("webforj.assetsSource", "packaged");
    keys.put("webforj.devtools.livereload.enabled", "true");
    keys.put("webforj.devtools.craftforj.enabled", "true");
    List<String> lines = new ArrayList<>();
    keys.forEach((key, value) -> {
      if (!Pattern.compile("(?m)^\\s*" + Pattern.quote(key) + "\\s*[=:]").matcher(text).find()) {
        lines.add(key + " = " + value);
      }
    });
    if (lines.isEmpty()) {
      return text;
    }

    String separator = text.isEmpty() || text.endsWith("\n") ? "" : "\n";
    String ending = text.isEmpty() || text.endsWith("\n") ? "\n" : "";

    return text + separator + String.join("\n", lines) + ending;
  }

  private static boolean isPom(Xml.Document document) {
    return document.getSourcePath().getFileName().toString().equals(POM);
  }

  private static Path getModule(Xml.Document document) {
    Path parent = document.getSourcePath().getParent();

    return parent == null ? Path.of("") : parent;
  }

  private static Optional<InstallSettings> findInstallSettings(Xml.Document document) {
    Xml.Tag build = document.getRoot().getChild("build").orElse(null);
    Xml.Tag plugins = build == null ? null : build.getChild("plugins").orElse(null);
    if (plugins == null) {
      return Optional.empty();
    }

    for (Xml.Tag plugin : plugins.getChildren(PLUGIN)) {
      if (plugin.getChildValue(ARTIFACT_ID).filter(INSTALL_PLUGIN::equals).isPresent()) {
        Xml.Tag configuration = plugin.getChild(CONFIGURATION_TAG).orElse(null);
        InstallSettings settings = new InstallSettings();
        if (configuration != null) {
          settings.deployUrl = configuration.getChildValue("deployurl").orElse(null);
          settings.publishName = configuration.getChildValue("publishname").orElse(null);
          settings.classname = configuration.getChildValue("classname").orElse(null);
          settings.debug = configuration.getChildValue("debug").orElse(null);
          settings.username = configuration.getChildValue("username").orElse(null);
          settings.password = configuration.getChildValue("password").orElse(null);
          settings.token = configuration.getChildValue("token").orElse(null);
        }

        settings.merged = plugins.getChildren(PLUGIN).stream().anyMatch(
            other -> other.getChildValue(ARTIFACT_ID).filter(BUILD_PLUGIN::equals).isPresent());

        settings.version = document.getRoot().getChild("dependencies")
            .flatMap(dependencies -> dependencies.getChildren("dependency").stream()
                .filter(dependency -> dependency.getChildValue(ARTIFACT_ID)
                    .filter("webforj"::equals).isPresent())
                .findFirst())
            .flatMap(dependency -> dependency.getChildValue("version")).orElse(null);

        return Optional.of(settings);
      }
    }

    return Optional.empty();
  }

  /**
   * The settings of the webforJ install plugin by module, and the files of the build.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Accumulator {
    private final Map<Path, InstallSettings> settings = new HashMap<>();
    private final Set<Path> files = new HashSet<>();
  }

  private static final class InstallSettings {
    private String deployUrl;
    private String publishName;
    private String classname;
    private String debug;
    private String version;
    private String username;
    private String password;
    private String token;
    private boolean merged;
  }

  private static final class PomMigration extends XmlIsoVisitor<ExecutionContext> {
    private final InstallSettings settings;

    PomMigration(InstallSettings settings) {
      this.settings = settings;
    }

    @Override
    public Xml.Tag visitTag(Xml.Tag tag, ExecutionContext ctx) {
      Xml.Tag visited = super.visitTag(tag, ctx);
      if ("dependencies".equals(visited.getName()) && isProjectChild()) {
        addEngine(visited);
      } else if (isPlugin(visited, INSTALL_PLUGIN)) {
        migrateInstallPlugin(visited);
      } else if (settings.merged && isPlugin(visited, BUILD_PLUGIN)) {
        mergeIntoBuildPlugin(visited);
      }

      return visited;
    }

    private void addEngine(Xml.Tag dependencies) {
      boolean declared =
          dependencies.getChildren("dependency").stream().anyMatch(dependency -> dependency
              .getChildValue(ARTIFACT_ID).filter("webforj-engine"::equals).isPresent());
      if (declared) {
        return;
      }

      String version =
          settings.version == null ? "" : "<version>" + settings.version + "</version>";
      doAfterVisit(
          new AddToTagVisitor<>(dependencies, Xml.Tag.build(ENGINE_DEPENDENCY.formatted(version))));
    }

    private void migrateInstallPlugin(Xml.Tag plugin) {
      if (settings.merged) {
        doAfterVisit(new RemoveContentVisitor<>(plugin, true, true));
        return;
      }

      Optional<Xml.Tag> existing = plugin.getChild(CONFIGURATION_TAG);
      if (existing.flatMap(configuration -> configuration.getChild("hotswap")).isPresent()) {
        return;
      }

      plugin.getChild("executions").ifPresent(
          executions -> doAfterVisit(new RemoveContentVisitor<>(executions, true, true)));
      existing.ifPresent(
          configuration -> doAfterVisit(new RemoveContentVisitor<>(configuration, true, true)));
      doAfterVisit(new AddToTagVisitor<>(plugin, createConfiguration()));
    }

    private void mergeIntoBuildPlugin(Xml.Tag plugin) {
      Optional<Xml.Tag> existing = plugin.getChild(CONFIGURATION_TAG);
      if (existing.isEmpty()) {
        doAfterVisit(new AddToTagVisitor<>(plugin, createConfiguration()));
        return;
      }

      for (Xml.Tag child : createConfiguration().getChildren()) {
        if (existing.get().getChild(child.getName()).isEmpty()) {
          doAfterVisit(new AddToTagVisitor<>(existing.get(), child));
        }
      }
    }

    private Xml.Tag createConfiguration() {
      return Xml.Tag.build("<configuration>" + formatSettings() + "</configuration>");
    }

    private static boolean isPlugin(Xml.Tag plugin, String artifactId) {
      return plugin.getChildValue(ARTIFACT_ID).filter(artifactId::equals).isPresent();
    }

    private boolean isProjectChild() {
      Object parent = getCursor().getParentTreeCursor().getValue();

      return parent instanceof Xml.Tag tag && "project".equals(tag.getName());
    }

    private String formatSettings() {
      StringBuilder bbj = new StringBuilder();
      if (settings.publishName != null) {
        bbj.append("<publishName>").append(settings.publishName).append("</publishName>");
      }

      if (settings.deployUrl != null) {
        bbj.append("<deployment><url>").append(toDeploymentUrl(settings.deployUrl))
            .append("</url></deployment>");
      }

      StringBuilder admin = new StringBuilder();
      appendTag(admin, "username", settings.username);
      appendTag(admin, "password", settings.password);
      appendTag(admin, "token", settings.token);
      if (!admin.isEmpty()) {
        bbj.append("<admin>").append(admin).append("</admin>");
      }

      String hotswap = "<hotswap><hotswapAgent/></hotswap>";

      return bbj.isEmpty() ? hotswap : hotswap + "<bbj>" + bbj + "</bbj>";
    }

    private static void appendTag(StringBuilder target, String name, String value) {
      if (value != null) {
        target.append('<').append(name).append('>').append(value).append("</").append(name)
            .append('>');
      }
    }

    private static String toDeploymentUrl(String deployUrl) {
      try {
        URI url = URI.create(deployUrl);
        if (url.getScheme() == null || url.getRawAuthority() == null) {
          return deployUrl;
        }

        return url.getScheme() + "://" + url.getRawAuthority() + DEPLOYMENT_PATH;
      } catch (IllegalArgumentException unparsable) {
        return deployUrl;
      }
    }
  }
}
