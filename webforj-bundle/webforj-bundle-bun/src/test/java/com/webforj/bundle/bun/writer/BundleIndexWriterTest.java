package com.webforj.bundle.bun.writer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.Gson;
import com.webforj.bundle.BundleIndex;
import com.webforj.bundle.BundleIndexDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BundleIndexWriterTest {

  @Test
  void shouldMapEachEntryToItsOutputsFromMetafile(@TempDir Path tmp) throws IOException {
    Path metafile = tmp.resolve("meta.json");
    Files.writeString(metafile, """
        {
          "outputs": {
            "./card/card-A1.js":      { "entryPoint": "card/card.ts" },
            "./card/card-C3.css":     { "entryPoint": "card/card.ts" },
            "./chunk-Z9.js":          { "imports": [] },
            "./asset-Q1.png":         { "imports": [] },
            "./counter/index-B2.js":  { "entryPoint": "counter/index.ts" }
          }
        }
        """);

    Map<String, List<String>> mapping = new BundleIndexWriter().mapOutputs(metafile, tmp);

    assertEquals(List.of("card/card-A1.js", "card/card-C3.css"), mapping.get("card/card.ts"));
    assertEquals(List.of("counter/index-B2.js"), mapping.get("counter/index.ts"));
    assertFalse(
        mapping.values().stream().flatMap(List::stream).anyMatch(f -> f.startsWith("chunk-")),
        "shared chunks carry no entry point and must be excluded");
  }

  @Test
  void shouldReturnEmptyMapWhenMetafileMissing(@TempDir Path tmp) throws IOException {
    assertTrue(new BundleIndexWriter().mapOutputs(tmp.resolve("absent.json"), tmp).isEmpty());
  }

  @Test
  void shouldBindEachClassToTheOutputsOfItsEntryKeys() {
    Map<String, List<String>> keyToFiles = new LinkedHashMap<>();
    keyToFiles.put("card/card.ts", List.of("card/card-A1.js", "card/card-C3.css"));
    keyToFiles.put("kitchen-sink", List.of("kitchen-sink-B2.js"));

    Map<String, Set<String>> classToKeys = new LinkedHashMap<>();
    classToKeys.put("CardView", new LinkedHashSet<>(List.of("card/card.ts")));
    classToKeys.put("KitchenSinkView",
        new LinkedHashSet<>(List.of("card/card.ts", "kitchen-sink")));
    classToKeys.put("UnbuiltView", new LinkedHashSet<>(List.of("missing")));

    Map<String, List<String>> bindings =
        new BundleIndexWriter().bindClasses(keyToFiles, classToKeys);

    assertEquals(List.of("card/card-A1.js", "card/card-C3.css"), bindings.get("CardView"));
    assertEquals(List.of("card/card-A1.js", "card/card-C3.css", "kitchen-sink-B2.js"),
        bindings.get("KitchenSinkView"));
    assertFalse(bindings.containsKey("UnbuiltView"),
        "a class whose keys produced no output is left out of the index");
  }

  @Test
  void shouldRoundTripThroughTheRuntimeIndex(@TempDir Path tmp) throws IOException {
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put("CardView", List.of("card/card-A1.js", "card/card-C3.css"));
    bindings.put("CounterView", List.of("counter/index-B2.js"));

    new BundleIndexWriter().write(tmp, bindings);
    BundleIndex index = readIndex(tmp);

    assertEquals(bindings, index.getBindings());
  }

  @Test
  void shouldRoundTripDebugFilesThroughTheRuntimeIndex(@TempDir Path tmp) throws IOException {
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put("CardView", List.of("card/card-A1.js", "panel/panel-D3.js"));
    bindings.put(BundleIndex.DEBUG_KEY, List.of("panel/panel-D3.js"));

    new BundleIndexWriter().write(tmp, bindings);
    BundleIndex index = readIndex(tmp);

    assertEquals(List.of("panel/panel-D3.js"), index.getDebugFiles());
    assertEquals(List.of("card/card-A1.js", "panel/panel-D3.js"),
        index.getBindings().get("CardView"));
    assertFalse(index.getBindings().containsKey(BundleIndex.DEBUG_KEY));
  }

  @Test
  void shouldRoundTripGlobalFilesThroughTheRuntimeIndex(@TempDir Path tmp) throws IOException {
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put("CardView", List.of("card/card-A1.js"));
    bindings.put(BundleIndex.GLOBAL_KEY, List.of("generated/theme/theme-G7.css"));

    new BundleIndexWriter().write(tmp, bindings);
    BundleIndex index = readIndex(tmp);

    assertEquals(List.of("generated/theme/theme-G7.css"), index.getGlobalFiles());
    assertEquals(List.of("card/card-A1.js"), index.getBindings().get("CardView"));
    assertFalse(index.getBindings().containsKey(BundleIndex.GLOBAL_KEY));
  }

  @Test
  void shouldWriteTheIndexUnderTheSingleResourcePath(@TempDir Path tmp) throws IOException {
    Path target = new BundleIndexWriter().write(tmp, Map.of("CardView", List.of("card.js")));

    assertEquals(BundleIndexDocument.RESOURCE,
        tmp.relativize(target).toString().replace('\\', '/'));
  }

  @Test
  void shouldLeaveTheIndexUntouchedWhenContentIsUnchanged(@TempDir Path tmp) throws IOException {
    BundleIndexWriter writer = new BundleIndexWriter();
    Map<String, List<String>> bindings = Map.of("CardView", List.of("card-A1.js"));

    Path target = writer.write(tmp, bindings);
    long firstStamp = Files.getLastModifiedTime(target).toMillis();

    Files.setLastModifiedTime(target, FileTime.fromMillis(firstStamp - 5000));
    long stale = Files.getLastModifiedTime(target).toMillis();

    writer.write(tmp, bindings);

    assertEquals(stale, Files.getLastModifiedTime(target).toMillis(),
        "an identical rebuild must not rewrite the file");
  }

  @Test
  void shouldRewriteTheIndexWhenContentChanges(@TempDir Path tmp) throws IOException {
    BundleIndexWriter writer = new BundleIndexWriter();

    Path target = writer.write(tmp, Map.of("CardView", List.of("card-A1.js")));
    String first = Files.readString(target);

    writer.write(tmp, Map.of("CardView", List.of("card-B2.js")));

    assertFalse(first.equals(Files.readString(target)), "a changed binding must rewrite the file");
  }

  private static BundleIndex readIndex(Path classesDir) throws IOException {
    String json = Files.readString(classesDir.resolve(BundleIndexDocument.RESOURCE));

    return new Gson().fromJson(json, BundleIndexDocument.class).toIndex();
  }
}
