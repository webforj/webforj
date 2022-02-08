package org.dwcj;

import com.basis.startup.type.CustomObject;

public interface IDwcjHelper {

    CustomObject getEventProxy(Object obj, String method);

    int msgbox(String msg, int options, String title);

}
