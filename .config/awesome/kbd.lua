 -- Keyboard layout widget
 kbd_widget = widget({ type = "textbox", name = "kbd_widget",
		       align = "right" })
 kbd_widget.border_width = 1
 -- kbd_widget.border_color = beautiful.fg_normal
 kbd_widget.text = " En "

 -- {{{ D-Bus
 dbus.request_name("session", "ru.gentoo.kbdd")
 dbus.add_match("session", "interface='ru.gentoo.kbdd',member='layoutChanged'")
 dbus.add_signal("ru.gentoo.kbdd", function(...)
				      local data = {...}
				      local layout = data[2]
				      lts = {[0] = "En", [1] = "Et", [2] = "Ru"}
				      kbd_widget.text = " "..lts[layout].." "
				   end
	      )
 --- }}}
