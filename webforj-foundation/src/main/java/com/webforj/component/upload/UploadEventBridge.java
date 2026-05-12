package com.webforj.component.upload;

import com.webforj.Page;
import com.webforj.UploadedFile;
import com.webforj.component.upload.event.UploadCompleteEvent;
import com.webforj.component.upload.event.UploadErrorEvent;
import com.webforj.component.upload.event.UploadListProgressEvent;
import com.webforj.component.upload.event.UploadProgressEvent;
import com.webforj.component.upload.event.UploadRejectEvent;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.function.BiConsumer;

/**
 * Routes client side {@code dwc-progress}, {@code dwc-list-progress}, {@code dwc-failed},
 * {@code dwc-rejected}, and {@code dwc-completed} events emitted by every {@code dwc-upload}
 * element on the page to the matching {@link Upload} component's event dispatcher.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
final class UploadEventBridge {
  private static final Map<Page, UploadEventBridge> BRIDGES = new WeakHashMap<>();
  private final Map<String, WeakReference<Upload>> uploadsByClientId = new HashMap<>();

  static synchronized UploadEventBridge ensure(Page page) {
    return BRIDGES.computeIfAbsent(page, UploadEventBridge::new);
  }

  private UploadEventBridge(Page page) {
    subscribe(page, "dwc-progress", this::dispatchProgress);
    subscribe(page, "dwc-list-progress", this::dispatchListProgress);
    subscribe(page, "dwc-failed", this::dispatchError);
    subscribe(page, "dwc-rejected", this::dispatchReject);
    subscribe(page, "dwc-completed", this::dispatchComplete);
  }

  synchronized void register(String clientId, Upload upload) {
    if (clientId == null || clientId.isEmpty()) {
      return;
    }

    uploadsByClientId.put(clientId, new WeakReference<>(upload));
  }

  synchronized void unregister(String clientId) {
    if (clientId == null) {
      return;
    }

    uploadsByClientId.remove(clientId);
  }

  private void subscribe(Page page, String type,
      BiConsumer<Upload, Map<String, Object>> dispatcher) {
    PageEventOptions opts = new PageEventOptions();
    opts.addData("clientId", "event.target && event.target.getAttribute('dwc-id')");
    opts.addData("fileName", "(event.detail && event.detail.file && event.detail.file.name) || ''");
    opts.addData("progress",
        "(event.detail && typeof event.detail.progress === 'number') ? event.detail.progress : -1");
    opts.addData("transferredBytes",
        "(event.detail && typeof event.detail.loaded === 'number') ? event.detail.loaded : -1");
    opts.addData("totalBytes",
        "(event.detail && typeof event.detail.total === 'number') ? event.detail.total : -1");
    opts.addData("listTotal",
        "(event.detail && typeof event.detail.listTotal === 'number') ? event.detail.listTotal : -1");
    opts.addData("listRemaining",
        "(event.detail && typeof event.detail.listRemaining === 'number') ? event.detail.listRemaining : -1");
    opts.addData("listTransferredBytes",
        "(event.detail && typeof event.detail.listTransferredBytes === 'number') ? event.detail.listTransferredBytes : -1");
    opts.addData("listTotalBytes",
        "(event.detail && typeof event.detail.listTotalBytes === 'number') ? event.detail.listTotalBytes : -1");
    opts.addData("listEntries",
        "(event.detail && Array.isArray(event.detail.entries)) ? event.detail.entries : []");
    opts.addData("message",
        "(event.detail && typeof event.detail.status === 'string') ? event.detail.status"
            + " : (event.detail && typeof event.detail.error === 'string') ? event.detail.error"
            + " : ''");
    opts.addData("uploadedNames", "(event.detail && Array.isArray(event.detail.uploaded))"
        + " ? event.detail.uploaded.map(f => f && f.name).filter(Boolean) : []");
    opts.addData("failedNames", "(event.detail && Array.isArray(event.detail.failed))"
        + " ? event.detail.failed.map(f => f && f.name).filter(Boolean) : []");
    opts.setFilter("event.target && event.target.tagName === 'DWC-UPLOAD'");

    page.addEventListener(type, ev -> route(ev, dispatcher), opts);
  }

  private void route(PageEvent ev, BiConsumer<Upload, Map<String, Object>> dispatcher) {
    Map<String, Object> data = ev.getData();
    Object clientId = data.get("clientId");
    if (clientId == null) {
      return;
    }

    String key = clientId.toString();
    Upload upload;
    synchronized (this) {
      WeakReference<Upload> ref = uploadsByClientId.get(key);
      upload = ref == null ? null : ref.get();
      if (ref != null && upload == null) {
        // The component was garbage collected without calling unregister.
        // Drop the stale entry to cleanup.
        uploadsByClientId.remove(key);
      }
    }

    if (upload != null) {
      dispatcher.accept(upload, data);
    }
  }

  private Map<String, Object> withSingleFile(Map<String, Object> data) {
    Object name = data.get("fileName");
    UploadedFile file = new UploadedFile(name == null ? "" : name.toString());
    Map<String, Object> enriched = new HashMap<>(data);
    enriched.put("files", List.of(file));

    return enriched;
  }

  private List<UploadedFile> toUploadedFiles(Object raw) {
    if (!(raw instanceof List<?> list)) {
      return List.of();
    }

    List<UploadedFile> out = new ArrayList<>();
    for (Object o : list) {
      if (o != null) {
        out.add(new UploadedFile(o.toString()));
      }
    }

    return out;
  }

  private void dispatchProgress(Upload upload, Map<String, Object> data) {
    upload.dispatchClientEvent(new UploadProgressEvent(upload, withSingleFile(data)));
  }

  private void dispatchListProgress(Upload upload, Map<String, Object> data) {
    Map<String, Object> enriched = new HashMap<>(data);
    enriched.put("entries", data.getOrDefault("listEntries", List.of()));
    upload.dispatchClientEvent(new UploadListProgressEvent(upload, enriched));
  }

  private void dispatchError(Upload upload, Map<String, Object> data) {
    upload.dispatchClientEvent(new UploadErrorEvent(upload, withSingleFile(data)));
  }

  private void dispatchReject(Upload upload, Map<String, Object> data) {
    upload.dispatchClientEvent(new UploadRejectEvent(upload, withSingleFile(data)));
  }

  private void dispatchComplete(Upload upload, Map<String, Object> data) {
    Map<String, Object> enriched = new HashMap<>(data);
    enriched.put("uploaded", toUploadedFiles(data.get("uploadedNames")));
    enriched.put("failed", toUploadedFiles(data.get("failedNames")));
    upload.dispatchClientEvent(new UploadCompleteEvent(upload, enriched));
  }
}
