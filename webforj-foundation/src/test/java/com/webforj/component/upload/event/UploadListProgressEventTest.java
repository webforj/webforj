package com.webforj.component.upload.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.upload.Upload;
import com.webforj.component.upload.event.UploadListProgressEvent.Entry;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class UploadListProgressEventTest {

  @Test
  void shouldExposeListAggregatesAndEntries() {
    Map<String, Object> payload = new HashMap<>();
    payload.put("listTotal", 3);
    payload.put("listRemaining", 1);
    payload.put("listTransferredBytes", 6_000L);
    payload.put("listTotalBytes", 10_000L);
    payload.put("entries",
        List.of(Map.of("name", "a.txt", "size", 1000, "loaded", 1000, "state", "complete"),
            Map.of("name", "b.txt", "size", 4000, "loaded", 5000, "state", "complete"),
            Map.of("name", "c.txt", "size", 5000, "loaded", 0, "state", "queued")));

    UploadListProgressEvent ev = new UploadListProgressEvent(new Upload(), payload);

    assertEquals(3, ev.getListTotal());
    assertEquals(1, ev.getListRemaining());
    assertEquals(6_000L, ev.getListTransferredBytes());
    assertEquals(10_000L, ev.getListTotalBytes());
    assertEquals(60.0, ev.getListProgress());

    List<Entry> entries = ev.getEntries();
    assertEquals(3, entries.size());
    assertEquals("a.txt", entries.get(0).getName());
    assertEquals(Entry.State.COMPLETE, entries.get(0).getState());
    assertEquals(Entry.State.QUEUED, entries.get(2).getState());
    assertEquals(0L, entries.get(2).getTransferredBytes());
  }

  @Test
  void shouldReturnDefaultsWhenPayloadIsMissing() {
    UploadListProgressEvent ev = new UploadListProgressEvent(new Upload(), new HashMap<>());

    assertEquals(-1, ev.getListTotal());
    assertEquals(-1, ev.getListRemaining());
    assertEquals(-1L, ev.getListTransferredBytes());
    assertEquals(-1L, ev.getListTotalBytes());
    assertEquals(-1.0, ev.getListProgress());
    assertTrue(ev.getEntries().isEmpty());
  }
}
