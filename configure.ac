AC_INIT([Haskell network-packet-linux package],
        [0.1.1.0])

AC_CONFIG_SRCDIR([Network/Socket/Linux/Types.hsc])

AC_CONFIG_HEADERS([include/PacketLinuxConfig.h])

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

AC_C_CONST

AC_CHECK_HEADERS_ONCE([linux/if_ether.h linux/if_packet.h])

AC_OUTPUT
