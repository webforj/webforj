package com.webforj.automation.see.component.infinitescroll;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.infinitescroll.InfiniteScroll;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes an infinite scroll with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class InfiniteScrollSnapshotSerializer implements SnapshotSerializer<InfiniteScroll> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<InfiniteScroll> getContract() {
    return InfiniteScroll.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }
}
