package com.webforj.bundle.bun.writer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bundle.bun.discovery.BundleEntryDeclaration;
import com.webforj.bundle.bun.plugin.BunPlugin;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BundleDriverWriterTest {

  private static BunPlugin plugin(String id) {
    return new BunPlugin().setId(id).setWrapper(id + "/" + id + ".mjs")
        .setWrapperContent(("export default () => ({ name: 'webforj-" + id + "' });")
            .getBytes(StandardCharsets.UTF_8));
  }

  @Test
  void shouldWriteTheDriverScript(@TempDir Path tmp) throws IOException {
    Path script = new BundleDriverWriter().writeDriver(tmp);

    assertEquals(tmp.resolve(BundleDriverWriter.DRIVER_NAME), script);
    String body = Files.readString(script);
    assertTrue(body.contains("Bun.build"), "driver must call Bun.build");
  }

  @Test
  void shouldWritePluginWrappersUnderTheirFolder(@TempDir Path tmp) throws IOException {
    new BundleDriverWriter().writePlugins(tmp, List.of(plugin("vue"), plugin("scss")));

    assertTrue(Files.isRegularFile(tmp.resolve("plugins/vue/vue.mjs")),
        "vue wrapper must be written");
    assertTrue(Files.isRegularFile(tmp.resolve("plugins/scss/scss.mjs")),
        "scss wrapper must be written");
    assertTrue(Files.readString(tmp.resolve("plugins/scss/scss.mjs")).contains("webforj-scss"));
  }

  @Test
  void shouldWriteNoWrapperFolderForNoPlugins(@TempDir Path tmp) throws IOException {
    new BundleDriverWriter().writePlugins(tmp, List.of());

    assertTrue(!Files.exists(tmp.resolve("plugins")), "no plugins must not create the folder");
  }

  @Test
  void shouldWriteConfigWithEntriesPluginsAndUserConfig(@TempDir Path tmp) throws IOException {
    BundleDriverWriter writer = new BundleDriverWriter();
    BundleDriverWriter.Config config = new BundleDriverWriter.Config()
        .setEntries(List.of(
            new BundleEntryDeclaration().setSource("card/card.ts").setBuildPath("card/card.ts")))
        .setOutdir("/out").setRoot("/root").setMetafile("/work/meta.json")
        .setEntryNaming("[dir]/[name]-[hash].[ext]").setMinify(true).setHashed(true)
        .setPlugins(List.of(plugin("vue"))).setUserConfig("/root/bun.config.ts");

    Path written = writer.writeConfig(tmp, config);

    assertEquals(tmp.resolve(BundleDriverWriter.CONFIG_NAME), written);
    String body = Files.readString(written);
    assertTrue(body.contains("\"card/card.ts\""), "entry source and build path must be serialized");
    assertTrue(body.contains("\"vue/vue.mjs\""), "plugin wrapper must be serialized");
    assertTrue(body.contains("\"vue\""), "plugin id must be serialized for options lookup");
    assertFalse(body.contains("webforj-vue"), "the wrapper source must not be serialized");
    assertTrue(body.contains("bun.config.ts"), "user config path must be serialized");
    assertTrue(body.contains("\"minify\": true"));
    assertTrue(body.contains("\"hashed\": true"));
  }

  @Test
  void shouldSerializeAbsentUserConfigAsNull(@TempDir Path tmp) throws IOException {
    BundleDriverWriter writer = new BundleDriverWriter();
    BundleDriverWriter.Config config = new BundleDriverWriter.Config()
        .setEntries(
            List.of(new BundleEntryDeclaration().setSource("a/a.ts").setBuildPath("a/a.ts")))
        .setOutdir("/out").setRoot("/root").setMetafile("/work/meta.json")
        .setEntryNaming("[dir]/[name].[ext]").setMinify(false).setHashed(false)
        .setPlugins(List.of()).setUserConfig(null);

    String body = Files.readString(writer.writeConfig(tmp, config));

    assertTrue(body.contains("\"userConfig\": null"), "absent user config must serialize as null");
  }
}
