package com.webforj.component.upload.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.webforj.UploadedFile;
import com.webforj.component.upload.Upload;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class UploadProgressEventTest {

  @Test
  void shouldExposeFileAndProgressFromPayload() {
    Map<String, Object> payload = new HashMap<>();
    payload.put("files", List.of(new UploadedFile("doc.pdf")));
    payload.put("transferredBytes", 4_200L);
    payload.put("totalBytes", 10_000L);

    UploadProgressEvent ev = new UploadProgressEvent(new Upload(), payload);

    assertNotNull(ev.getFile());
    assertEquals("doc.pdf", ev.getFile().getClientName());
    assertEquals(42.0, ev.getProgress()); // derived: 4200/10000 * 100
    assertEquals(4_200L, ev.getTransferredBytes());
    assertEquals(10_000L, ev.getTotalBytes());
  }

  @Test
  void shouldReturnDefaultsWhenPayloadFieldsAreMissing() {
    UploadProgressEvent ev = new UploadProgressEvent(new Upload(), new HashMap<>());

    assertNull(ev.getFile());
    assertEquals(-1.0, ev.getProgress());
    assertEquals(-1L, ev.getTransferredBytes());
    assertEquals(-1L, ev.getTotalBytes());
  }
}
