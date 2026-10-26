package com.webforj.testbed;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.Page;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.button.Button;
import com.webforj.exceptions.WebforjWebManagerException;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class DownloadTest extends TestbedTestSupport {
  @TempDir
  Path directory;

  @Test
  void shouldListTheFilesTheApplicationSentForDownload() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    byte[] report = "name,total".getBytes(StandardCharsets.UTF_8);
    Button export = new Button("Export");
    export.onClick(event -> Page.getCurrent().download(report, "report.csv"));
    context.add(export);

    UserInteraction.click(export);
    UserInteraction.click(export);

    List<DownloadedFile> downloads = context.getDownloads();
    assertEquals(List.of("report.csv", "report.csv"),
        downloads.stream().map(DownloadedFile::getName).toList());
    assertArrayEquals(report, downloads.get(0).getContents());
    assertEquals("report.csv (10 bytes)", downloads.get(0).toString());
  }

  @Test
  void shouldFailTheDownloadOfTheMissingFile() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    context.getFrame();
    File missing = directory.resolve("missing.csv").toFile();
    Page page = Page.getCurrent();

    assertThrows(WebforjWebManagerException.class, () -> page.download(missing, "report.csv"));

    assertEquals(List.of(), context.getDownloads());
  }
}
