package org.dwcj;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;

/**
 * AbstractWebStorageMock needed for Testing.
 */
public class AbstractWebStorageMock extends AbstractWebStorage {

  public AbstractWebStorageMock(BBjThinClient thinClient) {
    super(thinClient, WebStorageType.COOKIES);
  }

  private AbstractWebStorageMock() throws BBjException {
    super(Environment.getCurrent().getBBjAPI().getThinClient(), WebStorageType.COOKIES);
  }


}
