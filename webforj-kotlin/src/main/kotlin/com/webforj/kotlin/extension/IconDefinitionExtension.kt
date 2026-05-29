package com.webforj.kotlin.extension

import com.webforj.component.icons.IconDefinition

fun IconDefinition<*>.asString() = "$pool:$name"