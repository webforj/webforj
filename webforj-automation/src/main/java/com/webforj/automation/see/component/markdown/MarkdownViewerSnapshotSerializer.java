package com.webforj.automation.see.component.markdown;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.markdown.MarkdownViewer;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes a markdown viewer with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class MarkdownViewerSnapshotSerializer implements SnapshotSerializer<MarkdownViewer> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<MarkdownViewer> getContract() {
    return MarkdownViewer.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }
}
