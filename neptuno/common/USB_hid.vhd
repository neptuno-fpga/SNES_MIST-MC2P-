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
---------------------------------------------------------------------
-- USB HID  
-- Victor Trucco 2021 
-------------------------------------------------------------------

library IEEE; 
use IEEE.std_logic_1164.all; 
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all; 

entity usb_hid is
generic (
    divisor         : integer := 50 ); 
port (
    clk_i           : in std_logic;
    reset_i         : in std_logic;
    usb_rx_i        : in std_logic;

    mouse_x_o       : out std_logic_vector(7 downto 0);
    mouse_y_o       : out std_logic_vector(7 downto 0);
    mouse_z_o       : out std_logic_vector(7 downto 0);
    mouse_bts_o     : out std_logic_vector(7 downto 0);
    mouse_strobe_o  : out std_logic;  
    
    key_pressed_o   : out std_logic;                    --// 1-make (pressed), 0-break (released)
    key_extended_o  : out std_logic;                    --// extended code
    key_code_o      : out std_logic_vector(7 downto 0); --// key scan code
    key_strobe_o    : out std_logic;                    --// key data valid

    joy1_buttons_o    : out std_logic_vector(15 downto 0);
    joy2_buttons_o    : out std_logic_vector(15 downto 0)

);    
end usb_hid;

architecture rtl of usb_hid is

    type t_joy  is array (0 to 1) of std_logic_vector(63 downto 0);
    signal joy_data          : t_joy;
    --signal joy1_data       : std_logic_vector(63 downto 0)  := (others=>'0');
    --signal joy2_data       : std_logic_vector(63 downto 0)  := (others=>'0');

    signal joy1_slot    : std_logic_vector(7 downto 0) := x"ff";
    signal joy2_slot    : std_logic_vector(7 downto 0) := x"ff";

    signal joy_data_count  : integer range 0 to 63; --std_logic_vector(8 downto 0);
    signal joystick_present : unsigned(1 downto 0) := "00";


    signal count     : integer range 0 to 2048; --std_logic_vector(8 downto 0);
    signal timeout   : integer;
    signal data      : std_logic_vector(7 downto 0);
    signal ready     : std_logic;
    signal device_type : std_logic_vector(7 downto 0);
    signal device_id : std_logic_vector(7 downto 0);

    signal x         : std_logic_vector(7 downto 0) := "00000000";
    signal y         : std_logic_vector(7 downto 0) := "00000000";
    signal z         : std_logic_vector(7 downto 0) := "00000000";
    signal b         : std_logic_vector(7 downto 0) := "00000000";
    

    type t_key  is array (0 to 13) of std_logic_vector(7 downto 0);
    signal new_keys        : t_key;
    signal keys            : t_key;

    signal new_message_s : std_logic := '0';
    signal poll_msg : std_logic := '0';

    signal message_type    : std_logic_vector(7 downto 0);
    signal length   : std_logic_vector (15 downto 0);
    signal device    : std_logic_vector(7 downto 0);


    signal descriptor_buttons_ini : integer; 
    signal descriptor_buttons_qtd : integer; 

    signal mouse_strobe_s : std_logic := '0';

begin


    inst_rx : entity work.usb_receiver
    generic map (
        divisor     => divisor ) 
    port map (
        I_CLK       => clk_i,
        I_RESET     => reset_i,
        I_RX        => usb_rx_i,
        O_DATA      => data,
        O_READY     => ready
    );

-- TODO: we need to proper parse the HID. For now, just a quick and dirty hack, 
-- 01 is the "report 01" for some gamepads and we are only looking for 12 buttons 
 process (clk_i, joystick_present)
    variable u_v, d_v, l_v, r_v : std_logic := '0';
    variable x_ini : integer := 15;
    variable y_ini : integer := 23;
    begin

    if (joystick_present > "00") then

        for I in 0 to 1 loop
            if joy_data(I)(7 downto 0) = x"01" then --we have a report ID 
                x_ini := 31;
                y_ini := 39;
            else
                x_ini := 7;
                y_ini := 15;
            end if;

           if (joy_data(I)(x_ini downto x_ini-7) < x"70") then l_v := '1'; else l_v := '0'; end if;
           if (joy_data(I)(x_ini downto x_ini-7) > x"90") then r_v := '1'; else r_v := '0'; end if;
           if (joy_data(I)(y_ini downto y_ini-7) < x"70") then u_v := '1'; else u_v := '0'; end if;
           if (joy_data(I)(y_ini downto y_ini-7) > x"90") then d_v := '1'; else d_v := '0'; end if;

           if I = 0 then
               if joy1_slot = x"FF" then joy1_buttons_o <= (others=>'0'); else joy1_buttons_o <= joy_data(I)(55 downto 44) & u_v & d_v & l_v & r_v; end if; -- 12 buttons, directional
           else
               if joy2_slot = x"FF" then joy2_buttons_o <= (others=>'0'); else joy2_buttons_o <= joy_data(I)(55 downto 44) & u_v & d_v & l_v & r_v; end if;-- 12 buttons, directional
           end if;
        end loop;
    else
       joy1_buttons_o <= (others=>'0');
       joy2_buttons_o <= (others=>'0');
    end if;

end process;
    
    -- Mouse
    mouse_bts_o     <= b;
    mouse_x_o       <= x(7 downto 0);
    mouse_y_o       <= y(7 downto 0);
    mouse_z_o       <= z(7 downto 0);
    mouse_strobe_o  <= mouse_strobe_s;

    
    process (reset_i, clk_i, data, new_message_s, ready)
    variable data_v : std_logic_vector(7 downto 0);
    variable state_v : std_logic;
    variable ready_v : std_logic_vector(1 downto 0);
    begin
        if reset_i = '1' then
            x <= (others => '0');
            y <= (others => '0');
            z <= (others => '0');
            b <= (others => '0');
            new_message_s <= '1';
            joy_data(0)<= (others => '0');
            joy_data(1)<= (others => '0');
            
        elsif new_message_s = '1' then
            count <= 0;
            new_message_s <= '0';
            device_type <= x"00";
            device_id <= x"FF";
            timeout <= 0;
            poll_msg <= '0';
        elsif rising_edge(clk_i) then 

            ready_v := ready_v(0) & ready;

            if ready_v = "01" then -- rising edge ready

                timeout <= 0;
                if count = 0 then


                   if data = x"FA" or data = x"FB"  then --we are in sync, let´s parse the data
                        count <= count + 1;
                        joy_data_count <= 0;
                        if data = X"FA" then poll_msg <= '1'; else poll_msg <= '0'; end if;
                   end if;

                else
                
                
                    if count = 1 then
                       length(7 downto 0) <= data;

                    elsif count = 2 then
                        length(15 downto 8) <= data;

                    elsif count = 3 then 
                        message_type <= data;
                    --                                  #define MSG_TYPE_CONNECTED      0x01
                    --                                  #define MSG_TYPE_DISCONNECTED   0x02
                    --                                  #define MSG_TYPE_ERROR          0x03
                    --                                  #define MSG_TYPE_DEVICE_POLL    0x04 <---------- information on buttons press
                    --                                  #define MSG_TYPE_DEVICE_STRING  0x05
                    --                                  #define MSG_TYPE_DEVICE_INFO    0x06
                    --                                  #define MSG_TYPE_HID_INFO       0x07 <---------- descriptor with axis and buttons information
                    --                                  #define MSG_TYPE_STARTUP        0x08

                    elsif count = 4 then -- device type
                        device_type <= data; -- types are "UNKNOWN", "POINTER", "MOUSE", "RESERVED", "JOYSTICK", "GAMEPAD", "KEYBOARD", "KEYPAD", "MULTI_AXIS", "SYSTEM"

                    elsif count = 5 then 
                        device_id <= data; -- a ID signed by the system. Tipicaly 00, 01 or 02


--                      when 5 => device <= data;
--                      when 6 => endpoint <= data;
--                      when 7 => vendorL <= data;                         
--                      when 8 => vendorH <= data;                         
--                      when 9 => productL <= data;                         
--                      when 10 => productH <= data; 
                    end if;

                    if poll_msg = '0' then -- it's a USB message

                           if message_type = x"01" then -- MSG_TYPE_CONNECTED
                           -- at this point we don´t have much information about what was plugged in


                        elsif message_type = x"02" then -- MSG_TYPE_DISCONNECTED        

                                if device_type = x"04" and count = 6 then -- a joystick was disconnected
                                     if joy1_slot = device_id then
                                        joy1_slot <= x"FF";
                                        joystick_present <= joystick_present - 1;
                                     elsif joy2_slot = device_id then
                                        joy2_slot <= x"FF";
                                        joystick_present <= joystick_present - 1;
                                     end if;  
                                end if;        

                               

                        elsif message_type = x"07" then -- MSG_TYPE_HID_INFO
                            -- we need to parse the descriptor here

                        end if;

                    elsif poll_msg = '1' then -- it's a poll request
                       
                        if message_type = x"04" then -- MSG_TYPE_DEVICE_POLL

                            case device_type is

                                when x"04" =>    -- Joystick

                                    if (count=6) then
                                        if joy1_slot /= device_id and joy2_slot /= device_id then -- we have a new joystick present
                                             if joy1_slot = x"FF" then
                                                joy1_slot <= device_id;
                                             else
                                                joy2_slot <= device_id;
                                             end if;
                                            joystick_present <= joystick_present + 1;
                                        end if;
                                    end if;

                                     if (count>=11 and count<=18) then 
                                        if (joy1_slot = device_id) then
                                            joy_data(0)(joy_data_count+8-1 downto joy_data_count) <= data; 
                                        else
                                            joy_data(1)(joy_data_count+8-1 downto joy_data_count) <= data; 
                                        end if;

                                        joy_data_count <= joy_data_count + 8; 
                                     end if;

                                     if (count=19) then new_message_s <= '1'; end if;-- wait for a new message

  



                                when x"02" =>    -- Mouse         

                                    case count is
                                        when 11 => b <= data;                -- Buttons
                                        when 12 => x <= data; --x + data;    -- Left/Right delta
                                        when 13 => y <= data; --y + data;    -- Up/Down delta
                                        when 14 => z <= data; --z + data;    -- Wheel delta
                                                   mouse_strobe_s <= not mouse_strobe_s; new_message_s <= '1'; -- strobe the mouse and wait for a new message
                                        when others => null;
                                    end case;
                                    
                                when x"06" =>    -- Keyboard
                                    case count is
                                        when 11 => -- modifiers
                                                if (data(0)='1') then new_keys( 0) <= x"e0"; else new_keys( 0) <= x"00"; end if; -- left Control
                                                if (data(1)='1') then new_keys( 2) <= x"e1"; else new_keys( 2) <= x"00"; end if; -- Left shift (CAPS SHIFT)
                                                if (data(2)='1') then new_keys( 4) <= x"e2"; else new_keys( 4) <= x"00"; end if; -- Left Alt
                                                if (data(3)='1') then new_keys( 6) <= x"e3"; else new_keys( 6) <= x"00"; end if; -- Left Gui
                                                if (data(4)='1') then new_keys( 8) <= x"e4"; else new_keys( 8) <= x"00"; end if; -- CTRL (Symbol Shift)
                                                if (data(5)='1') then new_keys(10) <= x"e5"; else new_keys(10) <= x"00"; end if; -- Right shift (CAPS SHIFT)
                                                if (data(6)='1') then new_keys(12) <= x"e6"; else new_keys(12) <= x"00"; end if; -- Right Alt
                                                if (data(7)='1') then new_keys(13) <= x"e7"; else new_keys(13) <= x"00"; end if; -- Right Gui

    --                                    when 12 => -- reserved
                                        when 13 => new_keys(1) <= data; -- key 1
                                        when 14 => new_keys(3) <= data; -- key 2
                                        when 15 => new_keys(5) <= data; -- key 3
                                        when 16 => new_keys(6) <= data; -- key 4
                                        when 17 => new_keys(9) <= data; -- key 5
                                        when 18 => new_keys(11) <= data; -- key 6
                                        when 19 => new_message_s <= '1'; -- wait for a new message
                                        when others => null;
                                    end case;
                                
                                when others => null;
                            end case;
                        end if;

                    else -- it´s a discarted message
                        if data = x"0A" then --end of current message, let´s try the next
                            new_message_s <= '1';
                        end if;
                    end if;

                    count <= count + 1;
                end if;
            else
                timeout <= timeout + 1;
                if timeout > 10000 then --the message was stalled, let´s try another one
                    new_message_s <= '1';
                end if;
            end if; -- end ready = 1

            key_strobe_o   <= '0';                  

            for I in 0 to 13 loop
                if new_keys(I) /= keys(I) then

                    if new_keys(I) = X"00" then
                        data_v := keys(I); 
                        state_v := '0';
                    else
                        data_v := new_keys(I);
                        state_v := '1';
                    end if;

                    keys(I) <= new_keys(I);

                    key_strobe_o   <= '1';                   --// key data valid
                    exit;
                end if;
            end loop;

            key_extended_o  <= '0';                  --// default is not a extended code

            case data_v is
                when X"04" =>   key_code_o <= x"1c"; -- A
                when X"05" =>   key_code_o <= x"32"; -- B
                when X"06" =>   key_code_o <= x"21"; -- C
                when X"07" =>   key_code_o <= x"23"; -- D
                when X"08" =>   key_code_o <= x"24"; -- E
                when X"09" =>   key_code_o <= x"2B"; -- F
                when X"0a" =>   key_code_o <= x"34"; -- G
                when X"0b" =>   key_code_o <= x"33"; -- H
                when X"0c" =>   key_code_o <= x"43"; -- I
                when X"0d" =>   key_code_o <= x"3B"; -- J
                when X"0e" =>   key_code_o <= x"42"; -- K
                when X"0f" =>   key_code_o <= x"4B"; -- L
                when X"10" =>   key_code_o <= x"3A"; -- M
                when X"11" =>   key_code_o <= x"31"; -- N
                when X"12" =>   key_code_o <= x"44"; -- O
                when X"13" =>   key_code_o <= x"4D"; -- P
                when X"14" =>   key_code_o <= x"15"; -- Q
                when X"15" =>   key_code_o <= x"2D"; -- R
                when X"16" =>   key_code_o <= x"1B"; -- S
                when X"17" =>   key_code_o <= x"2C"; -- T
                when X"18" =>   key_code_o <= x"3C"; -- U
                when X"19" =>   key_code_o <= x"2A"; -- V
                when X"1a" =>   key_code_o <= x"1D"; -- W
                when X"1b" =>   key_code_o <= x"22"; -- X
                when X"1c" =>   key_code_o <= x"35"; -- Y
                when X"1D" =>   key_code_o <= x"1A"; -- Z

                when X"1E" =>   key_code_o <= x"16"; -- 1
                when X"1F" =>   key_code_o <= x"1E"; -- 2
                when X"20" =>   key_code_o <= x"26"; -- 3
                when X"21" =>   key_code_o <= x"25"; -- 4
                when X"22" =>   key_code_o <= x"2E"; -- 5
                when X"23" =>   key_code_o <= x"36"; -- 6
                when X"24" =>   key_code_o <= x"3D"; -- 7
                when X"25" =>   key_code_o <= x"3E"; -- 8
                when X"26" =>   key_code_o <= x"46"; -- 9
                when X"27" =>   key_code_o <= x"45"; -- 0

                when X"28" =>   key_code_o <= x"5a"; -- enter
                when X"29" =>   key_code_o <= x"76"; -- esc
                when X"2A" =>   key_code_o <= x"66"; -- backspace
                when X"2B" =>   key_code_o <= x"0d"; -- tab
                when X"2C" =>   key_code_o <= x"29"; -- space

                when X"2D" =>   key_code_o <= x"4E"; -- -
                when X"2E" =>   key_code_o <= x"55"; -- =
                when X"2F" =>   key_code_o <= x"5B"; -- [
                when X"30" =>   key_code_o <= x"5D"; -- ]
                when X"31" =>   key_code_o <= x"46"; -- \

                when X"33" =>   key_code_o <= x"4c"; -- ;
                when X"34" =>   key_code_o <= x"52"; -- '
                when X"35" =>   key_code_o <= x"0e"; -- `
                when X"36" =>   key_code_o <= x"41"; -- ,
                when X"37" =>   key_code_o <= x"49"; -- .
                when X"38" =>   key_code_o <= x"4A"; -- /

                when X"39" =>   key_code_o <= x"58"; -- CAPS LOCK
                when X"3a" =>   key_code_o <= x"05"; -- F1
                when X"3b" =>   key_code_o <= x"06"; -- F2
                when X"3c" =>   key_code_o <= x"04"; -- F3
                when X"3d" =>   key_code_o <= x"0c"; -- F4
                when X"3e" =>   key_code_o <= x"03"; -- F5
                when X"3f" =>   key_code_o <= x"0b"; -- F6
                when X"40" =>   key_code_o <= x"83"; -- F7
                when X"41" =>   key_code_o <= x"0a"; -- F8
                when X"42" =>   key_code_o <= x"01"; -- F9
                when X"43" =>   key_code_o <= x"09"; -- F10
                when X"44" =>   key_code_o <= x"78"; -- F11
                when X"45" =>   key_code_o <= x"07"; -- F12
                when X"46" =>   key_code_o <= x"7c"; key_extended_o  <= '1';  -- Print Screen
                when X"47" =>   key_code_o <= x"7E"; -- Scroll Lock
                -- pause 48 to E1 14 77 E1 F0 14 F0 77
                when X"49" =>   key_code_o <= x"70"; key_extended_o  <= '1';  -- Insert
                when X"4A" =>   key_code_o <= x"6c"; key_extended_o  <= '1';  -- Home
                when X"4B" =>   key_code_o <= x"7d"; key_extended_o  <= '1';  -- Pg Up
                when X"4C" =>   key_code_o <= x"71"; key_extended_o  <= '1';  -- Delete
                when X"4D" =>   key_code_o <= x"69"; key_extended_o  <= '1';  -- End
                when X"4E" =>   key_code_o <= x"7A"; key_extended_o  <= '1';  -- Pg Dw
                when X"4F" =>   key_code_o <= x"74"; key_extended_o  <= '1';  -- Right
                when X"50" =>   key_code_o <= x"6b"; key_extended_o  <= '1';  -- Left
                when X"51" =>   key_code_o <= x"72"; key_extended_o  <= '1';  -- Down
                when X"52" =>   key_code_o <= x"75"; key_extended_o  <= '1';  -- Up
                when X"53" =>   key_code_o <= x"77"; -- Num Lock

                when X"54" =>   key_code_o <= x"4a"; key_extended_o  <= '1'; -- keypad /
                when X"55" =>   key_code_o <= x"7c"; -- keypad *
                when X"56" =>   key_code_o <= x"7b"; -- keypad -
                when X"57" =>   key_code_o <= x"79"; -- keypad +
                when X"58" =>   key_code_o <= x"5a"; key_extended_o  <= '1'; -- keypad enter
                when X"59" =>   key_code_o <= x"69"; -- keypad 1
                when X"5a" =>   key_code_o <= x"72"; -- keypad 2
                when X"5b" =>   key_code_o <= x"7a"; -- keypad 3
                when X"5c" =>   key_code_o <= x"6b"; -- keypad 4
                when X"5d" =>   key_code_o <= x"73"; -- keypad 5
                when X"5e" =>   key_code_o <= x"74"; -- keypad 6
                when X"5f" =>   key_code_o <= x"6c"; -- keypad 7
                when X"60" =>   key_code_o <= x"75"; -- keypad 8
                when X"61" =>   key_code_o <= x"7d"; -- keypad 9
                when X"62" =>   key_code_o <= x"70"; -- keypad 0
                when X"63" =>   key_code_o <= x"71"; -- keypad .

                when X"e0" =>   key_code_o <= x"14"; -- left control
                when X"e1" =>   key_code_o <= x"12"; -- left shift
                when X"e2" =>   key_code_o <= x"11"; -- left alt
                when X"e3" =>   key_code_o <= x"1f"; key_extended_o  <= '1'; -- left GUI
                when X"e4" =>   key_code_o <= x"14"; key_extended_o  <= '1'; -- right control
                when X"e5" =>   key_code_o <= x"59"; -- right shift
                when X"e6" =>   key_code_o <= x"11"; key_extended_o  <= '1'; -- right alt
                when X"e7" =>   key_code_o <= x"27"; key_extended_o  <= '1'; -- right GUI

                when others =>  null;
            end case; 


            key_pressed_o   <= state_v;              --// 1-make (pressed), 0-break (released)
               




        end if;
    end process;

end architecture;
