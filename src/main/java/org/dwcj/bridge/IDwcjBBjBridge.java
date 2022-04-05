package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.CustomObject;

public interface IDwcjBBjBridge {

    CustomObject getEventProxy(Object obj, String method);

    CustomObject getEventProxy(Object obj, String method, String eventclassname);

    BBjControl createWidget(String classname, BBjWindow wnd);

    int msgbox(String msg, int options, String title);

    java.lang.Object invokeMethod(java.lang.Object object, java.lang.String method, java.util.ArrayList args);
}
