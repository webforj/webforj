package org.dwcj;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;

public class AbstractWebStorageMock extends AbstractWebStorage {

  private AbstractWebStorageMock() throws BBjException {
    super(Environment.getInstance().getBBjAPI().getThinClient(), WebStorageType.COOKIES);
  }

  public AbstractWebStorageMock(BBjThinClient thinClient) {
    super(thinClient, WebStorageType.COOKIES);
  }


}
