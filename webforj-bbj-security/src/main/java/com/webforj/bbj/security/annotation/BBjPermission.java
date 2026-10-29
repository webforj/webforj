package com.webforj.bbj.security.annotation;

import com.webforj.bbj.security.model.BBjPermissions;

/**
 * BBjPermission
 */
public @interface BBjPermission {

  BBjPermissions[] value();

}
