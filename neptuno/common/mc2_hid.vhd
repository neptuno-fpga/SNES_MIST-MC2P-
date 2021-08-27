--
-- Multicore 2 / Multicore 2+
--
-- Copyright (c) 2017-2021 - Victor Trucco
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- You are responsible for any legal issues arising from your use of this code.
--
-- Rev. 2021/02/01 - Oduvaldo (ducasp@gmail.com)
--      * This is a unified version, so it combine the old io_ps2_keyboard and
--        the old kbd_joystick components as well as the new 4 buttons component.
--        Those are going to be deprecated in the future, so please use this instead.
--      * This version also incorporates Roberto Focosi OSD control using front 
--        buttons, I've added debounce to the keys and made it also allow OSD
--        navigation (Green is Up, Red is Down, Blue is Enter, Yellow invokes or exit
--        OSD)
--
-- Rev. 2020/12/14 - Oduvaldo (ducasp@gmail.com)
--      * When using a Mega Drive 6 button controller MODE is a modifier for
--        all other buttons:
--           MODE + START -> F12 or OSD Invoke
--           MODE + (X, Y, Z, A, B, C) -> Keys 1, 2, 3, 4, 5, 6
--      * START now is a modifier for all other buttons as well:
--           START + (X, Y, Z, B) -> Keys 7, 8, 9, 0
--      * MODE and START work on their own functions if pressed alone
--      * Virtual Keypad for Player1 and Player2 available, 12 bits, mapped:
--           VKPx(11,10, 9 ,8,7,6,5,4,3,2,1,0)
--               (FN2,0,FN1,9,8,7,6,5,4,3,2,1)
--           Each joypad triggers it as:
--           MODE + (X, Y, Z, A, B, C) -> Keys 1, 2, 3, 4, 5, 6
--           START + (X, Y, Z, A, B, C) -> Keys 7, 8, 9, FN1, 0, FN2
--
-- Rev. 2020/10/12 - Oduvaldo (ducasp@gmail.com)
--      * When using a Mega Drive 6 button controller you can use the MODE
--        button to invoke OSD menu, on 8bitdo M30 the - button is the MODE 
--        button.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;
use IEEE.std_logic_unsigned.all;

entity MC2_HID is
generic
(
        OSD_CMD         : in   std_logic_vector(2 downto 0) := "001";
        CLK_SPEED       : integer := 50000;
        USE_VKP         : in std_logic := '0';
        use_usb_g       : in std_logic := '0'
);
port 
(
        Clk_i             : in std_logic;
        reset_i           : in std_logic := '0';

        kbd_clk_io        : inout std_logic;
        kbd_dat_io        : inout std_logic;
        
        keyb_pressed_o    : out std_logic;                    --// 1-make (pressed), 0-break (released)
        keyb_extended_o   : out std_logic;                    --// extended code
        keyb_code_o       : out std_logic_vector(7 downto 0); --// key scan code
        keyb_strobe_o     : out std_logic;                    --// toggles every key press/release
    
        mouse_x_o         : out std_logic_vector(7 downto 0);
        mouse_y_o         : out std_logic_vector(7 downto 0);
        mouse_z_o         : out std_logic_vector(7 downto 0);
        mouse_bts_o       : out std_logic_vector(7 downto 0);
        mouse_strobe_o    : out std_logic;                    --// toggles every move/click

--        kbd_interrupt   : out std_logic;
--        kbd_scancode    : out std_logic_vector(7 downto 0);
        usb_rx_i          : in std_logic := '0';

        joystick_0_i      : in std_logic_vector(5 downto 0);
        joystick_1_i      : in std_logic_vector(5 downto 0);

        -- joystick_0 and joystick_1 should be swapped
        joyswap_i         : in std_logic := '0';

        -- player1 and player2 should get both joystick_0 and joystick_1
        oneplayer_i       : in std_logic := '0';

        -- tilt, coin4-1, start4-1
        controls_o        : out std_logic_vector(8 downto 0);

        -- Function Keys
        F_keys_o          : out std_logic_vector(12 downto 1);

        direct_video_o    : out std_logic := '0';
        osd_rotate_o      : out std_logic_vector(1 downto 0) := "00";

        -- fire12-1, up, down, left, right
        player1_o         : out std_logic_vector(15 downto 0);
        player2_o         : out std_logic_vector(15 downto 0);

        -- virtual keypad generated by 6 buttons joysticks
        vkp1_o            : out std_logic_vector(11 downto 0);
        vkp2_o            : out std_logic_vector(11 downto 0);

        osd_o             : out   std_logic_vector(7 downto 0);
        osd_enable_i      : in std_logic;

        -- sega joystick
        sega_strobe_o     : out std_logic;

        -- Front buttons
        front_buttons_i   : in std_logic_vector(3 downto 0) := "1111";
        front_buttons_o   : out std_logic_vector(3 downto 0)
);
end MC2_HID;

architecture Behavioral of MC2_HID is

-- Front buttons
signal   fb_o_s             : std_logic_vector (3 downto 0) := (others => '1');
signal   fb_reset           : std_logic := '1';
signal   fb_osd             : std_logic_vector (7 downto 0) := (others => '1');
constant fb_reset_time      : integer := (CLK_SPEED*1500); -- 1.5s button for hold to reset
constant fb_osd_time        : integer := (CLK_SPEED*200); -- 200 ms - duration of the open OSD command

-- keyboard IO
constant timeout_const: integer range 0 to 63000 := (CLK_SPEED/4); -- consider transfer lost if more than 250us elapses between bits
constant debounce_const: integer range 0 to 250 := (CLK_SPEED/3333); -- 0,3us filter on clock
signal clk_filter: integer range 0 to 250;
signal bitsCount: integer range 0 to 10 := 0;
signal timeout: integer range 0 to 63000 := 0;
signal clk_reg_s            : std_logic;
signal clk_waitNextBit_s    : std_logic;
signal shift_reg_s          : std_logic_vector(10 downto 0) := (others => '0');
signal ps2_key_code_s     : std_logic_vector(7 downto 0) := (others => '0');
signal ps2_key_strobe_s   : std_logic := '0';
signal ps2_key_pressed_s  : std_logic; 
signal ps2_key_extended_s : std_logic; 
signal KbdClrInt_s        : std_logic := '0';
--

signal F_keys_s   : std_logic_vector(12 downto 1) := (others=>'0');

signal osd_s      : std_logic_vector(7 downto 0) := (others=>'1');
signal osd_sega   : std_logic_vector(7 downto 0) := (others=>'1');

-- keyboard controls
signal btn_tilt : std_logic := '0';
signal btn_one_player : std_logic := '0';
signal btn_two_players : std_logic := '0';
signal btn_three_players : std_logic := '0';
signal btn_four_players : std_logic := '0';
signal btn_left : std_logic := '0';
signal btn_right : std_logic := '0';
signal btn_down : std_logic := '0';
signal btn_up : std_logic := '0';
signal btn_fireA : std_logic := '0';
signal btn_fireB : std_logic := '0';
signal btn_fireC : std_logic := '0';
signal btn_fireD : std_logic := '0';
signal btn_fireE : std_logic := '0';
signal btn_fireF : std_logic := '0';
signal btn_fireG : std_logic := '0';
signal btn_fireH : std_logic := '0';
signal btn_fireI : std_logic := '0';
signal btn_coin  : std_logic := '0';
signal btn_start1_mame : std_logic := '0';
signal btn_start2_mame : std_logic := '0';
signal btn_start3_mame : std_logic := '0';
signal btn_start4_mame : std_logic := '0';
signal btn_coin1_mame : std_logic := '0';
signal btn_coin2_mame : std_logic := '0';
signal btn_coin3_mame : std_logic := '0';
signal btn_coin4_mame : std_logic := '0';
signal btn_up2 : std_logic := '0';
signal btn_down2 : std_logic := '0';
signal btn_left2 : std_logic := '0';
signal btn_right2 : std_logic := '0';
signal btn_fire2A : std_logic := '0';
signal btn_fire2B : std_logic := '0';
signal btn_fire2C : std_logic := '0';
signal btn_fire2D : std_logic := '0';
signal btn_fire2E : std_logic := '0';
signal btn_fire2F : std_logic := '0';
signal btn_fire2G : std_logic := '0';
signal btn_fire2H : std_logic := '0';
signal btn_fire2I : std_logic := '0';

--signal btn_scroll : std_logic := '0';

signal joy0 : std_logic_vector(5 downto 0);
signal joy1 : std_logic_vector(5 downto 0);

signal p1 : std_logic_vector(15 downto 0);
signal p2 : std_logic_vector(15 downto 0);

-- sega
signal clk_sega_s : std_logic := '0';
signal clk_delay : unsigned(9 downto 0) := (others=>'1');
signal TIMECLK   : integer;

signal joyP7_s : std_logic := '0';
signal sega1_s : std_logic_vector(11 downto 0) := (others=>'1');
signal sega2_s : std_logic_vector(11 downto 0) := (others=>'1');
signal segaf1_s : std_logic_vector(11 downto 0) := (others=>'1');
signal segaf2_s : std_logic_vector(11 downto 0) := (others=>'1');
signal sega1_vkp_s : std_logic_vector(11 downto 0) := (others=>'0');
signal mvkp1 : std_logic_vector(11 downto 0) := (others=>'0');
signal sega2_vkp_s : std_logic_vector(11 downto 0) := (others=>'0');
signal mvkp2 : std_logic_vector(11 downto 0) := (others=>'0');

signal osd_rotate_s : std_logic_vector(1 downto 0);
signal direct_video_s : std_logic := '1';


signal usb_key_code_s : std_logic_vector(7 downto 0);
signal usb_key_pressed_s : std_logic := '0';
signal usb_key_extended_s : std_logic := '0';
signal usb_key_strobe_s : std_logic := '0';

signal keyb_strobe_s : std_logic := '1';


signal usb_joy1_buttons_s : std_logic_vector(15 downto 0) := (others=>'0');
signal usb_joy2_buttons_s : std_logic_vector(15 downto 0) := (others=>'0');

begin 

g_usb : if use_usb_g = '1' generate
    inst_usb : entity work.usb_hid
    generic map ( divisor   => (CLK_SPEED/980) )  
    port map
    (
        clk_i               => clk_i,
        reset_i             => reset_i,
        usb_rx_i            => usb_rx_i,
        
        mouse_x_o           => mouse_x_o,
        mouse_y_o           => mouse_y_o,
        mouse_z_o           => mouse_z_o,
        mouse_bts_o         => mouse_bts_o,
        mouse_strobe_o      => mouse_strobe_o,
       
        key_pressed_o       => usb_key_pressed_s,  --// 1-make (pressed), 0-break (released)
        key_extended_o      => usb_key_extended_s, --// extended code
        key_code_o          => usb_key_code_s,     --// key scan code
        key_strobe_o        => usb_key_strobe_s,   --// key data valid

        joy1_buttons_o      => usb_joy1_buttons_s,
        joy2_buttons_o      => usb_joy2_buttons_s
    );  
end generate;


process (clk_i)
variable strobe_v : std_logic_vector(1 downto 0) := "00";
begin 
    if rising_edge(clk_i) then
        strobe_v := strobe_v(0) & (usb_key_strobe_s or ps2_key_strobe_s);
        if strobe_v = "01" and osd_enable_i = '0' then
            keyb_strobe_s <=  not keyb_strobe_s;
        end if;
    end if;
end process;

keyb_pressed_o   <= ps2_key_pressed_s; 
keyb_extended_o  <= ps2_key_extended_s; 
keyb_code_o      <= ps2_key_code_s; 
keyb_strobe_o    <= keyb_strobe_s;  

osd_rotate_o <= osd_rotate_s;
direct_video_o <= direct_video_s;
front_buttons_o <= fb_reset & fb_o_s(2 downto 0) when osd_enable_i = '0' else fb_reset & "111";

joy0 <= joystick_1_i when joyswap_i = '1' else joystick_0_i;
joy1 <= joystick_0_i when joyswap_i = '1' else joystick_1_i;

mvkp1 <= sega2_vkp_s when joyswap_i = '1' else sega1_vkp_s;
vkp1_o <= mvkp1;
mvkp2 <= sega1_vkp_s when joyswap_i = '1' else sega2_vkp_s;
vkp2_o <= mvkp2;

controls_o <= (btn_tilt or mvkp1(8)) &
              (btn_coin or btn_coin4_mame or mvkp1(7)) & 
              (btn_coin or btn_coin3_mame or mvkp1(6)) &
              (btn_coin or btn_coin2_mame or mvkp1(5)) & 
              (btn_coin or btn_coin1_mame or mvkp1(4)) &
              (btn_start4_mame or mvkp1(3)) & 
              (btn_start3_mame or mvkp1(2)) & 
              (btn_start2_mame or mvkp1(1)) &
              (btn_start1_mame or mvkp1(0));

p1 <= ("0000" & (not segaf1_s)) or 
      ("000" & btn_fireI & btn_fireH  &  btn_fireG  & btn_fireF  & btn_fireE  & btn_fireD  & btn_fireC  & btn_fireB  & btn_fireA  & btn_up  & btn_down  & btn_left  & btn_right)   
      or ( usb_joy1_buttons_s )
            when osd_enable_i = '0' else (others=>'0');


p2 <= ("0000" & (not segaf2_s)) or 
      ("000" & btn_fire2I & btn_fire2H &  btn_fire2G & btn_fire2F & btn_fire2E & btn_fire2D & btn_fire2C & btn_fire2B & btn_fire2A & btn_up2 & btn_down2 & btn_left2 & btn_right2)
      or (  usb_joy2_buttons_s )
            when osd_enable_i = '0' else (others=>'0');


player1_o <= p1 or p2 when oneplayer_i = '1' else p1;
player2_o <= p1 or p2 when oneplayer_i = '1' else p2;

osd_o <= osd_s and fb_osd and osd_sega
         and not("000" & (usb_joy1_buttons_s(4) or usb_joy1_buttons_s(5) or usb_joy1_buttons_s(6) or usb_joy1_buttons_s(7)) & usb_joy1_buttons_s(0) & usb_joy1_buttons_s(1) & usb_joy1_buttons_s(2) & usb_joy1_buttons_s(3)) 
         and not("000" & (usb_joy2_buttons_s(4) or usb_joy2_buttons_s(5) or usb_joy2_buttons_s(6) or usb_joy2_buttons_s(7)) & usb_joy2_buttons_s(0) & usb_joy2_buttons_s(1) & usb_joy2_buttons_s(2) & usb_joy2_buttons_s(3))    
            when osd_enable_i = '1' else (osd_s(7 downto 5) and osd_sega(7 downto 5) and fb_osd(7 downto 5)) & "11111";

--osd_o <= "111" & (not usb_joy_buttons_s(4 downto 0));

F_keys_o <= F_keys_s;

bt1 : entity work.debounce port map(clk_i, front_buttons_i(0), fb_o_s(0));
bt2 : entity work.debounce port map(clk_i, front_buttons_i(1), fb_o_s(1));
bt3 : entity work.debounce port map(clk_i, front_buttons_i(2), fb_o_s(2));
bt4 : entity work.debounce port map(clk_i, front_buttons_i(3), fb_o_s(3));

-- Front buttons debouncing process
process(clk_i)
  variable cnt_r : integer := 0;
  variable osd_on : integer := 0;
begin
  if rising_edge(clk_i) then

    -- And this is the reset specific check
    if (fb_o_s(3) = '0' and fb_reset = '1') then
        if (cnt_r = 0) then
            cnt_r := fb_reset_time;
        elsif (cnt_r = 1) then
            cnt_r := 0;
            fb_reset <= '0';
        else
            cnt_r := cnt_r - 1;
        end if;
    elsif (fb_o_s(3) = '1' and fb_reset = '0') then
        fb_reset <= '1';
        cnt_r := 0;
    elsif (fb_o_s(3) = '1' and cnt_r /= 0) then -- the button was released before the reset time, so it´s an "open OSD"
        osd_on := fb_osd_time;
        cnt_r := 0;
    else
        cnt_r := 0;
    end if;

    -- This is what sends the OSD invoking
--    if (fb_o_s(3) = '0' and fb_reset = '1') then
    if osd_on /= 0 then
        osd_on := osd_on - 1;
        fb_osd(7 downto 5) <= OSD_CMD; -- OSD Menu command
    else
        fb_osd(7 downto 5) <= "111";
    end if;

    -- And this is what sends UP (0), DOWN (2) and enter (1)
    if (osd_enable_i = '1' and fb_reset = '1') then
        fb_osd(0) <= fb_o_s(0);
        fb_osd(1) <= fb_o_s(2);
        fb_osd(4) <= fb_o_s(1);
    elsif (osd_enable_i = '1' and fb_reset = '0') then
        fb_osd(7 downto 5) <= OSD_CMD; -- OSD Menu command to disable it after reset
        fb_osd(4 downto 0) <= "11111";
    else
        fb_osd(4 downto 0) <= "11111";
    end if;
  end if;
end process;

ps2_keyboard : entity work.ps2_keyboard
    port map 
    (
        clock_i         => clk_i,
        reset_i         => '0',
        -- LEDs
        led_caps_i      => '0',
        -- PS/2 interface
        ps2_clk_io      => kbd_clk_io,
        ps2_data_io     => kbd_dat_io,
        -- Direct Access
        keyb_valid_o    => ps2_key_strobe_s,
        keyb_data_o     => ps2_key_code_s,
        --
        reset_o         => open,            
        por_o           => open,            
        reload_core_o   => open,          
        extra_keys_o    => open
    );


-- PS2 Keyboard Scan Conversion
process(clk_i)
variable KbdScanCode : std_logic_vector (7 downto 0);
variable IsReleased : std_logic_vector (1 downto 0);
begin
  if rising_edge(clk_i) then
    if ps2_key_strobe_s = '1' or usb_key_strobe_s = '1' then

            if (usb_key_strobe_s = '1') then
                KbdScanCode := usb_key_code_s;
                IsReleased(1) := not usb_key_pressed_s;
                ps2_key_extended_s <= usb_key_extended_s;
            else
                KbdScanCode := ps2_key_code_s;
                if (KbdScanCode = "11110000") then IsReleased(0) := '1';  else IsReleased(0) := '0';          end if;  
                if (KbdScanCode = x"e0")      then ps2_key_extended_s <= '1'; else ps2_key_extended_s <= '0'; end if; 
            end if;

           

            if KbdScanCode = x"05" then F_keys_s(1)       <= not(IsReleased(1)); end if; -- F1
            if KbdScanCode = x"06" then F_keys_s(2)       <= not(IsReleased(1)); end if; -- F2
            if KbdScanCode = x"04" then F_keys_s(3)       <= not(IsReleased(1)); end if; -- F3
            if KbdScanCode = x"0C" then F_keys_s(4)       <= not(IsReleased(1)); end if; -- F4
            if KbdScanCode = x"78" then F_keys_s(11)      <= not(IsReleased(1)); if osd_enable_i = '1' and  IsReleased(1) = '0'  then osd_rotate_s <= osd_rotate_s + 1; end if; end if; -- F11

            if KbdScanCode = x"7E" then if IsReleased(1) = '0' then direct_video_s <= not direct_video_s;   end if; end if; -- Scroll Lock

            if KbdScanCode = x"75" then btn_up            <= not(IsReleased(1)); end if; -- up
            if KbdScanCode = x"72" then btn_down          <= not(IsReleased(1)); end if; -- down
            if KbdScanCode = x"6B" then btn_left          <= not(IsReleased(1)); end if; -- left
            if KbdScanCode = x"74" then btn_right         <= not(IsReleased(1)); end if; -- right
            if KbdScanCode = x"76" then btn_coin          <= not(IsReleased(1)); end if; -- ESC

            if KbdScanCode = x"12" then btn_fireD         <= not(IsReleased(1)); end if; -- l-shift
            if KbdScanCode = x"14" then btn_fireC         <= not(IsReleased(1)); end if; -- ctrl
            if KbdScanCode = x"11" then btn_fireB         <= not(IsReleased(1)); end if; -- alt
            if KbdScanCode = x"29" then btn_fireA         <= not(IsReleased(1)); end if; -- Space
            if KbdScanCode = x"1A" then btn_fireE         <= not(IsReleased(1)); end if; -- Z
            if KbdScanCode = x"22" then btn_fireF         <= not(IsReleased(1)); end if; -- X
            if KbdScanCode = x"21" then btn_fireG         <= not(IsReleased(1)); end if; -- C
            if KbdScanCode = x"2A" then btn_fireH         <= not(IsReleased(1)); end if; -- V
            if KbdScanCode = x"32" then btn_fireI         <= not(IsReleased(1)); end if; -- B
            if KbdScanCode = x"66" then btn_tilt          <= not(IsReleased(1)); end if; -- Backspace

            -- JPAC/IPAC/MAME Style Codes
            if KbdScanCode = x"16" then btn_start1_mame   <= not(IsReleased(1)); end if; -- 1
            if KbdScanCode = x"1E" then btn_start2_mame   <= not(IsReleased(1)); end if; -- 2
            if KbdScanCode = x"26" then btn_start3_mame   <= not(IsReleased(1)); end if; -- 3
            if KbdScanCode = x"25" then btn_start4_mame   <= not(IsReleased(1)); end if; -- 4
            if KbdScanCode = x"2E" then btn_coin1_mame    <= not(IsReleased(1)); end if; -- 5
            if KbdScanCode = x"36" then btn_coin2_mame    <= not(IsReleased(1)); end if; -- 6
            if KbdScanCode = x"3D" then btn_coin3_mame    <= not(IsReleased(1)); end if; -- 7
            if KbdScanCode = x"3E" then btn_coin4_mame    <= not(IsReleased(1)); end if; -- 8
            --if KbdScanCode = x"2D" then btn_up2           <= not(IsReleased); end if; -- R
            --if KbdScanCode = x"2B" then btn_down2         <= not(IsReleased); end if; -- F
            --if KbdScanCode = x"23" then btn_left2         <= not(IsReleased); end if; -- D
            --if KbdScanCode = x"34" then btn_right2        <= not(IsReleased); end if; -- G
            --if KbdScanCode = x"1C" then btn_fire2A        <= not(IsReleased); end if; -- A
            --if KbdScanCode = x"1B" then btn_fire2B        <= not(IsReleased); end if; -- S
            --if KbdScanCode = x"15" then btn_fire2C        <= not(IsReleased); end if; -- Q
            --if KbdScanCode = x"1D" then btn_fire2D        <= not(IsReleased); end if; -- W
            --if KbdScanCode = x"43" then btn_fire2E        <= not(IsReleased); end if; -- I
            --if KbdScanCode = x"42" then btn_fire2F        <= not(IsReleased); end if; -- K
            --if KbdScanCode = x"3B" then btn_fire2G        <= not(IsReleased); end if; -- J
            --if KbdScanCode = x"4B" then btn_fire2H        <= not(IsReleased); end if; -- L
            
            -- OSD
            osd_s (4 downto 0) <= "11111";
            if (IsReleased(1) = '0') then  
                    if KbdScanCode = x"75" then osd_s(4 downto 0) <= "11110"; end if; -- up    arrow : 0x75
                    if KbdScanCode = x"72" then osd_s(4 downto 0) <= "11101"; end if; -- down  arrow : 0x72
                    if KbdScanCode = x"6b" then osd_s(4 downto 0) <= "11011"; end if; -- left  arrow : 0x6B
                    if KbdScanCode = x"74" then osd_s(4 downto 0) <= "10111"; end if; -- right arrow : 0x74
                    if KbdScanCode = x"5A" then osd_s(4 downto 0) <= "01111"; end if; -- ENTER
                        
                    if KbdScanCode = x"1c" then osd_s(4 downto 0) <= "00000"; end if;   -- A
                    if KbdScanCode = x"32" then osd_s(4 downto 0) <= "00001"; end if;   -- B
                    if KbdScanCode = x"21" then osd_s(4 downto 0) <= "00010"; end if;   -- C
                    if KbdScanCode = x"23" then osd_s(4 downto 0) <= "00011"; end if;   -- D
                    if KbdScanCode = x"24" then osd_s(4 downto 0) <= "00100"; end if;   -- E
                    if KbdScanCode = x"2b" then osd_s(4 downto 0) <= "00101"; end if;   -- F
                    if KbdScanCode = x"34" then osd_s(4 downto 0) <= "00110"; end if;   -- G
                    if KbdScanCode = x"33" then osd_s(4 downto 0) <= "00111"; end if;   -- H
                    if KbdScanCode = x"43" then osd_s(4 downto 0) <= "01000"; end if;   -- I
                    if KbdScanCode = x"3b" then osd_s(4 downto 0) <= "01001"; end if;   -- J
                    if KbdScanCode = x"42" then osd_s(4 downto 0) <= "01010"; end if;   -- K
                    if KbdScanCode = x"4b" then osd_s(4 downto 0) <= "01011"; end if;   -- L
                    if KbdScanCode = x"3a" then osd_s(4 downto 0) <= "01100"; end if;   -- M
                    if KbdScanCode = x"31" then osd_s(4 downto 0) <= "01101"; end if;   -- N
                    if KbdScanCode = x"44" then osd_s(4 downto 0) <= "01110"; end if;   -- O
                    if KbdScanCode = x"4d" then osd_s(4 downto 0) <= "10000"; end if;   -- P
                    if KbdScanCode = x"15" then osd_s(4 downto 0) <= "10001"; end if;   -- Q
                    if KbdScanCode = x"2d" then osd_s(4 downto 0) <= "10010"; end if;   -- R
                    if KbdScanCode = x"1b" then osd_s(4 downto 0) <= "10011"; end if;   -- S
                    if KbdScanCode = x"2c" then osd_s(4 downto 0) <= "10100"; end if;   -- T
                    if KbdScanCode = x"3c" then osd_s(4 downto 0) <= "10101"; end if;   -- U
                    if KbdScanCode = x"2a" then osd_s(4 downto 0) <= "10110"; end if;   -- V
                    if KbdScanCode = x"1d" then osd_s(4 downto 0) <= "11000"; end if;   -- W
                    if KbdScanCode = x"22" then osd_s(4 downto 0) <= "11001"; end if;   -- X
                    if KbdScanCode = x"35" then osd_s(4 downto 0) <= "11010"; end if;   -- Y
                    if KbdScanCode = x"1a" then osd_s(4 downto 0) <= "11100"; end if;   -- Z
                    
            end if;
            
            if (KbdScanCode = x"07" and IsReleased(1) = '0') or  -- key F12
               (KbdScanCode = x"76" and IsReleased(1) = '0' and osd_enable_i = '1') then -- ESC to abort an opened menu
                osd_s(7 downto 5) <= OSD_CMD; -- OSD Menu command
            else
                osd_s(7 downto 5) <= "111"; -- release
            end if;

            ps2_key_pressed_s <= not IsReleased(1);

            IsReleased := IsReleased(0) & '1';

            
    end if;

  end if;
end process;


--- Joystick read with sega 6 button support----------------------

process(clk_i)
begin
    if rising_edge(clk_i) then

        
        TIMECLK <= (9 * (CLK_SPEED/1000)); -- calculate ~9us from the master clock

        clk_delay <= clk_delay - 1;
        
        if (clk_delay = 0) then
            clk_sega_s <= not clk_sega_s;
            clk_delay <= to_unsigned(TIMECLK,10); 
        end if;

    end if;
end process;


process(clk_i)
    variable state_v : unsigned(8 downto 0) := (others=>'0');
    variable j1_sixbutton_v : std_logic := '0';
    variable j2_sixbutton_v : std_logic := '0';
    variable sega_edge : std_logic_vector(1 downto 0);
begin
    if rising_edge(clk_i) then

        sega_edge := sega_edge(0) & clk_sega_s;

        if sega_edge = "01" then
            state_v := state_v + 1;
            
            case state_v is
                -- joy_s format MXYZ SACB UDLR
                
                when '0'&X"01" =>  
                    joyP7_s <= '0';
                    
                when '0'&X"02" =>  
                    joyP7_s <= '1';
                    
                when '0'&X"03" => 
                    sega1_s(5 downto 0) <= joy0(5 downto 0); -- C, B, up, down, left, right 
                    sega2_s(5 downto 0) <= joy1(5 downto 0);        
                    
                    j1_sixbutton_v := '0'; -- Assume it's not a six-button controller
                    j2_sixbutton_v := '0'; -- Assume it's not a six-button controller

                    joyP7_s <= '0';

                when '0'&X"04" =>
                    if joy0(0) = '0' and joy0(1) = '0' then -- it's a megadrive controller
                                sega1_s(7 downto 6) <= joy0(5 downto 4); -- Start, A
                    else
                                sega1_s(7 downto 4) <= "11" & joy0(5 downto 4); -- It's an Atari or Master System controller (overwrite B and C)
                    end if;
                            
                    if joy1(0) = '0' and joy1(1) = '0' then -- it's a megadrive controller
                                sega2_s(7 downto 6) <= joy1(5 downto 4); -- Start, A
                    else
                                sega2_s(7 downto 4) <= "11" & joy1(5 downto 4); -- It's an Atari or Master System controller (overwrite B and C)
                    end if;
                    
                                        
                    joyP7_s <= '1';
            
                when '0'&X"05" =>  
                    joyP7_s <= '0';
                    
                when '0'&X"06" =>
                    if joy0(2) = '0' and joy0(3) = '0' then 
                        j1_sixbutton_v := '1'; --it's a Sega six button
                    end if;
                    
                    if joy1(2) = '0' and joy1(3) = '0' then 
                        j2_sixbutton_v := '1'; --it's a Sega six button
                    end if;
                    
                    joyP7_s <= '1';
                    
                when '0'&X"07" =>
                    if j1_sixbutton_v = '1' then
                        sega1_s(11 downto 8) <= joy0(0) & joy0(1) & joy0(2) & joy0(3); -- Mode, X, Y e Z                        
                    end if;

                    if j2_sixbutton_v = '1' then
                        sega2_s(11 downto 8) <= joy1(0) & joy1(1) & joy1(2) & joy1(3); -- Mode, X, Y e Z
                    end if;

                    if (sega1_s(11) = '0' and sega1_s(7) = '0') or (sega2_s(11) = '0' and sega2_s(7) = '0') then
                        osd_sega <= (OSD_CMD & (sega1_s(4) and sega1_s(5) and sega1_s(6)) & sega1_s(0) & sega1_s(1) & sega1_s(2) & sega1_s(3));
                    else
                        osd_sega <= ("111" & (sega1_s(4) and sega1_s(5) and sega1_s(6)) & sega1_s(0) & sega1_s(1) & sega1_s(2) & sega1_s(3));
                    end if;
                    joyP7_s <= '0';

                when others =>
                    joyP7_s <= '1';
                    
            end case;
                if USE_VKP = '1' then
                    sega1_vkp_s(0)  <= (not sega1_s(10)) and sega1_s(7)       and (not sega1_s(11));
                    sega1_vkp_s(1)  <= (not sega1_s(9))  and sega1_s(7)       and (not sega1_s(11));
                    sega1_vkp_s(2)  <= (not sega1_s(8))  and sega1_s(7)       and (not sega1_s(11));
                    sega1_vkp_s(3)  <= (not sega1_s(6))  and sega1_s(7)       and (not sega1_s(11));
                    sega1_vkp_s(4)  <= (not sega1_s(4))  and sega1_s(7)       and (not sega1_s(11));
                    sega1_vkp_s(5)  <= (not sega1_s(5))  and sega1_s(7)       and (not sega1_s(11));
                    sega1_vkp_s(6)  <= (not sega1_s(10)) and (not sega1_s(7)) and sega1_s(11);
                    sega1_vkp_s(7)  <= (not sega1_s(9))  and (not sega1_s(7)) and sega1_s(11);
                    sega1_vkp_s(8)  <= (not sega1_s(8))  and (not sega1_s(7)) and sega1_s(11);
                    sega1_vkp_s(9)  <= (not sega1_s(6))  and (not sega1_s(7)) and sega1_s(11);
                    sega1_vkp_s(10) <= (not sega1_s(4))  and (not sega1_s(7)) and sega1_s(11);
                    sega1_vkp_s(11) <= (not sega1_s(5))  and (not sega1_s(7)) and sega1_s(11);
                    sega2_vkp_s(0)  <= (not sega2_s(10)) and sega2_s(7)       and (not sega2_s(11));
                    sega2_vkp_s(1)  <= (not sega2_s(9))  and sega2_s(7)       and (not sega2_s(11));
                    sega2_vkp_s(2)  <= (not sega2_s(8))  and sega2_s(7)       and (not sega2_s(11));
                    sega2_vkp_s(3)  <= (not sega2_s(6))  and sega2_s(7)       and (not sega2_s(11));
                    sega2_vkp_s(4)  <= (not sega2_s(4))  and sega2_s(7)       and (not sega2_s(11));
                    sega2_vkp_s(5)  <= (not sega2_s(5))  and sega2_s(7)       and (not sega2_s(11));
                    sega2_vkp_s(6)  <= (not sega2_s(10)) and (not sega2_s(7)) and sega2_s(11);
                    sega2_vkp_s(7)  <= (not sega2_s(9))  and (not sega2_s(7)) and sega2_s(11);
                    sega2_vkp_s(8)  <= (not sega2_s(8))  and (not sega2_s(7)) and sega2_s(11);
                    sega2_vkp_s(9)  <= (not sega2_s(6))  and (not sega2_s(7)) and sega2_s(11);
                    sega2_vkp_s(10) <= (not sega2_s(4))  and (not sega2_s(7)) and sega2_s(11);
                    sega2_vkp_s(11) <= (not sega2_s(5))  and (not sega2_s(7)) and sega2_s(11);
                    segaf1_s <= sega1_s;
                    segaf2_s <= sega2_s;
                    --- Filter virtual keypad presses to not be detected as regular key presses
                    if (sega1_vkp_s(0) = '1' or sega1_vkp_s( 6) = '1') then segaf1_s(10) <= '1'; end if;
                    if (sega1_vkp_s(1) = '1' or sega1_vkp_s( 7) = '1') then segaf1_s( 9) <= '1'; end if;
                    if (sega1_vkp_s(2) = '1' or sega1_vkp_s( 8) = '1') then segaf1_s( 8) <= '1'; end if;
                    if (sega1_vkp_s(3) = '1' or sega1_vkp_s( 9) = '1') then segaf1_s( 6) <= '1'; end if;
                    if (sega1_vkp_s(4) = '1' or sega1_vkp_s(10) = '1') then segaf1_s( 4) <= '1'; end if;
                    if (sega1_vkp_s(5) = '1' or sega1_vkp_s(11) = '1') then segaf1_s( 5) <= '1'; end if;
                    if (sega2_vkp_s(0) = '1' or sega2_vkp_s( 6) = '1') then segaf2_s(10) <= '1'; end if;
                    if (sega2_vkp_s(1) = '1' or sega2_vkp_s( 7) = '1') then segaf2_s( 9) <= '1'; end if;
                    if (sega2_vkp_s(2) = '1' or sega2_vkp_s( 8) = '1') then segaf2_s( 8) <= '1'; end if;
                    if (sega2_vkp_s(3) = '1' or sega2_vkp_s( 9) = '1') then segaf2_s( 6) <= '1'; end if;
                    if (sega2_vkp_s(4) = '1' or sega2_vkp_s(10) = '1') then segaf2_s( 4) <= '1'; end if;
                    if (sega2_vkp_s(5) = '1' or sega2_vkp_s(11) = '1') then segaf2_s( 5) <= '1'; end if;
                else
                    segaf1_s <= sega1_s;
                    segaf2_s <= sega2_s;
                end if;

        end if;
    end if;
end process;

sega_strobe_o <= joyP7_s;
---------------------------

end Behavioral;


