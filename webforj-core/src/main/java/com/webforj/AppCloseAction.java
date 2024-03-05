package com.webforj;

/**
 * Provides an interface to configure the termination and Error actions of an application.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
// @formatter:off
public sealed interface AppCloseAction
    permits DefaultAction, NoneAction, MessageAction, RedirectAction {
// @formatter:on
}
