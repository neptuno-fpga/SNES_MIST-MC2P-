//============================================================================
//  SNES top-level for MiST
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================
//
//============================================================================
//
//  Multicore 2+ Top by Victor Trucco
//
//============================================================================

`default_nettype none

module SNES_MIST_TOP
(
    // Clocks
    input wire  clock_50_i,

    // SRAM (IS61WV102416BLL-10TLI)
    output [19:0]sram_addr_o = 20'b00000000000000000000,
    inout  [15:0]sram_data_io = 8'hZZZZ,
    output sram_we_n_o = 1'b1,
    output sram_oe_n_o = 1'b1,
    output sram_ub_n_o = 1'b1,
    output sram_lb_n_o = 1'b1,
	 
    // SDRAM (W9825G6KH-6)
    output [12:0] SDRAM_A,
    output  [1:0] SDRAM_BA,
    inout  [15:0] SDRAM_DQ,
    output        SDRAM_DQMH,
    output        SDRAM_DQML,
    output        SDRAM_CKE,
    output        SDRAM_nCS,
    output        SDRAM_nWE,
    output        SDRAM_nRAS,
    output        SDRAM_nCAS,
    output        SDRAM_CLK,

    // PS2
    inout wire  ps2_clk_io        = 1'bz,
    inout wire  ps2_data_io       = 1'bz,
    inout wire  ps2_mouse_clk_io  = 1'bz,
    inout wire  ps2_mouse_data_io = 1'bz,

    // SD Card
    output wire sd_cs_n_o         = 1'bZ,
    output wire sd_sclk_o         = 1'bZ,
    output wire sd_mosi_o         = 1'bZ,
    input wire  sd_miso_i,

    // Joysticks
    output wire joy_clock_o       = 1'b1,
    output wire joy_load_o        = 1'b1,
    input  wire joy_data_i,
    output wire joy_p7_o          = 1'b1,

    // Audio
    output      AUDIO_L,
    output      AUDIO_R,
    input wire  ear_i,
    //output wire mic_o             = 1'b0,

    // I2S Audio	 
    output        LRCLK,
    output        SDIN,
    output        SCLK,	 	 
	 
    // VGA
    output  [5:0] VGA_R,
    output  [5:0] VGA_G,
    output  [5:0] VGA_B,
    output        VGA_HS,
    output        VGA_VS,

    //STM32
    input wire  stm_tx_i,
    output wire stm_rx_o,
    output wire stm_rst_o           = 1'bz, // '0' to hold the microcontroller reset line, to free the SD card

    input         SPI_SCK,
    output        SPI_DO,
    input         SPI_DI,
    input         SPI_SS2,
    //output wire   SPI_nWAIT        = 1'b1, // '0' to hold the microcontroller data streaming

    //inout [31:0] GPIO,

    output LED                    = 1'b1 // '0' is LED on

);

//---------------------------------------------------------
//-- MC2+ defaults
//---------------------------------------------------------
//assign GPIO = 32'bzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;
assign stm_rst_o    = 1'bZ;
assign stm_rx_o = 1'bZ;
assign LED  = ~ioctl_download & ~bk_ena;

//all the SD reading goes thru the microcontroller for this core
assign sd_cs_n_o = 1'bZ;
assign sd_sclk_o = 1'bZ;
assign sd_mosi_o = 1'bZ;

wire joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i, joy1_p6_i, joy1_p9_i;
wire joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i, joy2_p6_i, joy2_p9_i;


neptuno_joydecoder  neptuno_joydecoder 
(
    .clk_i           ( clock_50_i ),
    .joy_data_i      ( joy_data_i ),
    .joy_clk_o       ( joy_clock_o ),
    .joy_load_o      ( joy_load_o ),

    .joy1_up_o       ( joy1_up_i ),
    .joy1_down_o     ( joy1_down_i ),
    .joy1_left_o     ( joy1_left_i ),
    .joy1_right_o    ( joy1_right_i ),
    .joy1_fire1_o    ( joy1_p6_i ),
    .joy1_fire2_o    ( joy1_p9_i ),

    .joy2_up_o       ( joy2_up_i ),
    .joy2_down_o     ( joy2_down_i ),
    .joy2_left_o     ( joy2_left_i ),
    .joy2_right_o    ( joy2_right_i ),
    .joy2_fire1_o    ( joy2_p6_i ),
    .joy2_fire2_o    ( joy2_p9_i )
);

// ROM Data Pump
reg [7:0] pump_s = 8'b11111111;
PumpSignal PumpSignal (clk_sys, ~locked, ioctl_download, pump_s);

//-----------------------------------------------------------------

parameter CONF_STR = {
    "P,LOAD_DATA;",
    "S0,SFC/SMC/BIN,Load Game...;",
//    "TF,Write Save RAM;",
    "OE,Video Region,NTSC,PAL;",
    "OAB,Scanlines,Off,25%,50%,75%;",
    "OG,Blend,On,Off;",
    "O9,Scandoubler,On,Off;",
    "O12,ROM Type,Auto,LoROM,HiROM,ExHiROM;",
//    "O56,Mouse,None,Port1,Port2;",
//    "OP,Super Scope,Off,Mouse;",
    "OM,Joy1 Mode,SG/MD,SNES;",
    "ON,Joy2 Mode,SG/MD,SNES;",
    "O7,Swap Joysticks,No,Yes;",
    //"OH,Multitap,Disabled,Port2;",
    "T0,Reset;",
    "V,v1.0.",`BUILD_DATE,"-mc2+"
};

wire [1:0] LHRom_type = status[2:1];
wire [1:0] scanlines = status[11:10];
wire       video_region = status[14];
wire [1:0] mouse_mode = status[6:5];
wire       joy1_mode = ~status[22];
wire       joy2_mode = ~status[23];
wire       joy_swap = status[7];
wire       multitap = status[17];
wire       BLEND = ~status[16];
wire       GUN_MODE = status[25];

////////////////////   CLOCKS   ///////////////////

wire locked;
wire clk_sys, clk_mem;

pll pll
(
    .inclk0(clock_50_i),
    .c0(SDRAM_CLK),
    .c1(clk_mem),
    .c2(clk_sys),
    .locked(locked)
);

//wire clk_25m2,clk_40;
//pll_vga pll_vga(
//    .inclk0(clock_50_i),
//    .c0(clk_25m2),
//    .c1(clk_40)
//    );

reg reset;
always @(posedge clk_sys) begin
    reset <= ~btn_n_o[4] | status[0] | ioctl_download;
end

//////////////////   MiST I/O   ///////////////////
wire  [9:0] conf_str_addr;
wire  [7:0] conf_str_char;

wire [31:0] joystick0;
wire [31:0] joystick1;
wire [31:0] joystick2;
wire [31:0] joystick3;
wire [31:0] joystick4;

wire  [1:0] buttons;
wire [31:0] status;
wire        ypbpr;
wire        scandoubler_disable;

reg         ioctl_wrD;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire [31:0] ioctl_filesize;
wire [15:0] ioctl_dout;
wire        ioctl_download;
wire  [7:0] ioctl_index;
wire        ioctl_wait;

wire  [8:0] mouse_x;
wire  [8:0] mouse_y;
wire  [7:0] mouse_flags;  // YOvfl, XOvfl, dy8, dx8, 1, mbtn, rbtn, lbtn
wire        mouse_strobe;

wire        ioctl_downl;
reg  [31:0] sd_lba;
reg         sd_rd = 0;
reg         sd_wr = 0;
wire        sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din;
wire        sd_buff_wr;
wire        sd_buff_rd;
wire        img_mounted;
wire [31:0] img_size;

wire [24:0] ps2_mouse = { mouse_strobe_level, mouse_y[7:0], mouse_x[7:0], mouse_flags };
reg         mouse_strobe_level;

always @(posedge clk_sys) begin
    conf_str_char <= CONF_STR[(($size(CONF_STR)>>3) - conf_str_addr - 1)<<3 +:8];
    if (mouse_strobe) mouse_strobe_level <= ~mouse_strobe_level;
end

data_io_snes #(
    .STRLEN(($size(CONF_STR)>>3)))
data_io_snes
(
    .clk_sys(clk_sys),
    .SPI_SCK(SPI_SCK),
    .SPI_DI(SPI_DI),
    .SPI_SS2(SPI_SS2),
    .SPI_DO        ( SPI_DO       ),

    .data_in        ( keys_s & pump_s ),
    .conf_str       ( CONF_STR ),
    .status         ( status ),

    .ioctl_wr(ioctl_wr),
    .ioctl_addr(ioctl_addr),
    .ioctl_dout(ioctl_dout),
    .ioctl_download(ioctl_downl),
    .ioctl_index(ioctl_index),
    .ioctl_filesize(ioctl_filesize),

    //--------------------------------------------

    .clk_sd(clk_mem),
    .sd_conf(1'b0),
    .sd_sdhc(1'b1),
    .sd_lba(sd_lba),
    .sd_rd({1'b0,sd_rd}),
    .sd_wr({1'b0,sd_wr}),
    .sd_ack(sd_ack),
    .sd_buff_addr(sd_buff_addr),
    .sd_dout(sd_buff_dout),
    .sd_din(sd_buff_din),
    .sd_dout_strobe(sd_buff_wr),
    .sd_din_strobe(sd_buff_rd),
    .img_mounted(img_mounted),
    .img_size(img_size)
);

assign ioctl_download = (ioctl_downl & ioctl_index == 8'h00) ? 1 : 0;

always @(posedge clk_sys) ioctl_wrD <= ioctl_wr;

////////////////////////////  SDRAM  ///////////////////////////////////

wire [23:0] ROM_ADDR;
//wire [23:1] rom_addr_rw = cart_download ? ioctl_addr[23:1] : ROM_ADDR[23:1];
wire [24:0] ioctl_addr_adj = ioctl_addr - ioctl_filesize[9:0]; // adjust for 512 byte SMC header
wire [23:1] rom_addr_rw = cart_download ? ioctl_addr_adj[23:1] : ROM_ADDR[23:1];
reg  [23:1] rom_addr_sd;
wire        ROM_CE_N;
wire        ROM_OE_N;
wire        ROM_WORD;
wire [15:0] ROM_Q = (ROM_WORD || ~ROM_ADDR[0]) ? rom_dout : { rom_dout[7:0], rom_dout[15:8] };
wire [15:0] rom_dout;
reg         rom_req;

wire [16:0] WRAM_ADDR;
reg  [16:0] wram_addr_sd;
wire        WRAM_CE_N;
wire        WRAM_OE_N;
wire        WRAM_RD_N;
wire        WRAM_WE_N;
wire  [7:0] WRAM_Q = WRAM_ADDR[0] ? wram_dout[15:8] : wram_dout[7:0];
wire  [7:0] WRAM_D;
reg   [7:0] wram_din;
wire [15:0] wram_dout;
wire        wram_rd = ~WRAM_CE_N & ~WRAM_RD_N;
reg         wram_rdD;
wire        wram_wr = ~WRAM_CE_N & ~WRAM_WE_N;
reg         wram_wrD;
wire        wram_req;

wire [19:0] BSRAM_ADDR;
reg  [19:0] bsram_sd_addr;
wire        BSRAM_CE_N;
wire        BSRAM_OE_N;
wire        BSRAM_WE_N;
wire        BSRAM_RD_N;
wire  [7:0] BSRAM_Q = BSRAM_ADDR[0] ? bsram_dout[15:8] : bsram_dout[7:0];
wire  [7:0] BSRAM_D;
reg   [7:0] bsram_din;
wire [15:0] bsram_dout;
wire        bsram_rd = ~BSRAM_CE_N & (~BSRAM_RD_N || rom_type[7:4] == 4'hC);
reg         bsram_rdD;
wire        bsram_wr = ~BSRAM_CE_N & ~BSRAM_WE_N;
reg         bsram_wrD;
wire        bsram_req;
reg         bsram_req_reg;

wire        VRAM_OE_N;

wire [15:0] VRAM1_ADDR;
reg  [14:0] vram1_addr_sd;
wire        VRAM1_WE_N;
wire  [7:0] VRAM1_D, VRAM1_Q;
reg   [7:0] vram1_din;
wire        vram1_req;
reg         vram1_req_reg;
reg         vram1_we_nD;

wire [15:0] VRAM2_ADDR;
reg  [14:0] vram2_addr_sd;
wire        VRAM2_WE_N;
wire  [7:0] VRAM2_D, VRAM2_Q;
reg   [7:0] vram2_din;
wire        vram2_req;
reg         vram2_req_reg;
reg         vram2_we_nD;

wire [15:0] ARAM_ADDR;
reg  [15:0] aram_addr_sd;
wire        ARAM_CE_N;
wire        ARAM_OE_N;
wire        ARAM_WE_N;
//wire  [7:0] ARAM_Q = ARAM_ADDR[0] ? aram_dout[15:8] : aram_dout[7:0];
wire  [7:0] ARAM_D;
reg   [7:0] aram_din;
wire [15:0] aram_dout;
wire        aram_rd = ~ARAM_CE_N & ~ARAM_OE_N;
reg         aram_rd_last;
wire        aram_wr = ~ARAM_CE_N & ~ARAM_WE_N;
reg         aram_wr_last;
wire        aram_req;
wire        aram_req_reg;

wire        DOT_CLK_CE;

always @(negedge clk_sys) begin

    reg ioctl_wr_last;

    ioctl_wr_last <= ioctl_wr;

    if ((~cart_download && ~ROM_CE_N /*&& ~ROM_OE_N */&& rom_addr_sd != rom_addr_rw) || ((/*ioctl_wr_last ^ */ioctl_wr) & cart_download)) begin
        rom_req <= ~rom_req;
        rom_addr_sd <= rom_addr_rw;
    end

    if (reset) begin
//      aram_addr_sd <= 16'haaaa;
//      wram_addr_sd <= 17'h1aaaa;
//      vram1_addr_sd <= 15'h7fff;
//      vram2_addr_sd <= 15'h7fff;
    end else begin

        wram_rdD <= wram_rd;
        wram_wrD <= wram_wr;
        if ((wram_rd && WRAM_ADDR[16:1] != wram_addr_sd[16:1]) || (~wram_wrD & wram_wr) || (~wram_rdD & wram_rd)) begin
            wram_req <= ~wram_req;
            wram_addr_sd <= WRAM_ADDR;
            wram_din <= WRAM_D;
        end

        bsram_rdD <= bsram_rd;
        bsram_wrD <= bsram_wr;
        if ((bsram_rd && BSRAM_ADDR[19:1] != bsram_sd_addr[19:1]) || (~bsram_wrD & bsram_wr) || (~bsram_rdD & bsram_rd)) begin
            bsram_req <= ~bsram_req;
            bsram_sd_addr <= BSRAM_ADDR;
            bsram_din <= BSRAM_D;
        end

        aram_wr_last <= aram_wr;
        aram_rd_last <= aram_rd;
        if ((aram_rd && ARAM_ADDR[15:1] != aram_addr_sd[15:1]) || (aram_wr && ARAM_ADDR != aram_addr_sd) || (aram_rd & ~aram_rd_last) || (aram_wr & ~aram_wr_last)) begin
            aram_req <= ~aram_req;
            aram_addr_sd <= ARAM_ADDR;
            aram_din <= ARAM_D;
        end

        vram1_we_nD <= VRAM1_WE_N;
        if ((vram1_we_nD & ~VRAM1_WE_N) || (VRAM1_ADDR[14:0] != vram1_addr_sd && ~VRAM_OE_N)) begin
            vram1_addr_sd <= VRAM1_ADDR[14:0];
            vram1_din <= VRAM1_D;
            vram1_req <= ~vram1_req;
        end

        vram2_we_nD <= VRAM2_WE_N;
        if ((vram2_we_nD & ~VRAM2_WE_N) || (VRAM2_ADDR[14:0] != vram2_addr_sd && ~VRAM_OE_N)) begin
            vram2_addr_sd <= VRAM2_ADDR[14:0];
            vram2_din <= VRAM2_D;
            vram2_req <= ~vram2_req;
        end
    end

end

sdram sdram
(
    .*,
    .init_n(locked),
    .clk(clk_mem),
    .clkref(DOT_CLK_CE),

    .rom_addr(rom_addr_sd),
    .rom_din(ioctl_dout),
    .rom_dout(rom_dout),
    .rom_req(rom_req),
    .rom_req_ack(),
    .rom_we(cart_download),

    .wram_addr(wram_addr_sd),
    .wram_din(WRAM_D),
    .wram_dout(wram_dout),
    .wram_req(wram_req),
    .wram_req_ack(),
    .wram_we(wram_wrD),

    .bsram_addr(bsram_sd_addr),
    .bsram_din(BSRAM_D),
    .bsram_dout(bsram_dout),
    .bsram_req(bsram_req),
    .bsram_req_ack(),
    .bsram_we(bsram_wrD),

    .bsram_io_addr(BSRAM_IO_ADDR),
    .bsram_io_din(BSRAM_IO_D),
    .bsram_io_dout(BSRAM_IO_Q),
    .bsram_io_req(bsram_io_req),
    .bsram_io_req_ack(),
    .bsram_io_we(bk_load),

    .vram1_req(0),//vram1_req),
    .vram1_ack(),
    .vram1_addr(vram1_addr_sd),
    .vram1_din(vram1_din),
    .vram1_dout(),//VRAM1_Q),
    .vram1_we(0),//~vram1_we_nD),

    .vram2_req(0),//vram2_req),
    .vram2_ack(),
    .vram2_addr(vram2_addr_sd),
    .vram2_din(vram2_din),
    .vram2_dout(),//VRAM2_Q),
    .vram2_we(0),//~vram2_we_nD),

    .aram_addr(aram_addr_sd),
    .aram_din(aram_din),
    .aram_dout(aram_dout),
    .aram_req(0),//aram_req),
    .aram_req_ack(),
    .aram_we(0),//aram_wr_last)
);


dpram #(15) vram1
(
    .clock(clk_sys),
    .address_a(VRAM1_ADDR[14:0]),
    .data_a(VRAM1_D),
    .wren_a(~VRAM1_WE_N),
    .q_a(VRAM1_Q),

    // clear the RAM on loading
    .address_b(ioctl_addr[14:0]),
    .wren_b(ioctl_wr & cart_download)
);

/*
dpram #(15) vram2
(
    .clock(clk_sys),
    .address_a(VRAM2_ADDR[14:0]),
    .data_a(VRAM2_D),
    .wren_a(~VRAM2_WE_N),
    .q_a(VRAM2_Q),

    // clear the RAM on loading
    .address_b(ioctl_addr[14:0]),
    .wren_b(ioctl_wr & cart_download)
);
*/
wire  [7:0] ARAM_Q;
dpram #(16) aram //audio RAM
(
    .clock(clk_sys),
    .address_a(ARAM_ADDR),
    .data_a(ARAM_D),
    .wren_a(~ARAM_CE_N & ~ARAM_WE_N),
    .q_A(ARAM_Q),

    // clear the RAM on loading
    .address_b(ioctl_addr[15:0]),
    .wren_b(ioctl_wr & cart_download)
);




// -- vram2 on SRAM --
wire sram_w_n = VRAM2_WE_N;
assign sram_oe_n_o  = 1'b0;
assign sram_data_io = (cart_download) ? 8'b00000000 : (~sram_w_n) ? VRAM2_D :  8'bzzzzzzzz;
assign sram_addr_o  = (cart_download) ? {6'b000000, ioctl_addr[14:0]} : {6'b000000, VRAM2_ADDR};
assign VRAM2_Q = sram_data_io;

reg [1:0] sram_rw;
reg last_wr, last_wr2;
always @(posedge clk_sys)
begin
    last_wr <= sram_w_n;
    last_wr2 <= last_wr;
  //  if ((~sram_w_n & last_wr) || (ioctl_wr & cart_download)) sram_rw <= 2'b00; else sram_rw <= {sram_rw[0],1'b1};
  //  sram_we_n_o <= sram_rw[1];

    if ((~sram_w_n & last_wr) || (ioctl_wr & cart_download)) sram_we_n_o <= 1'b0; else sram_we_n_o <= 1'b1;

end
// -------------------


assign SDRAM_CKE = 1'b1;

//////////////////////////  ROM DETECT  /////////////////////////////////

wire cart_download = ioctl_download;

reg        PAL;
reg  [7:0] rom_type;
reg  [7:0] rom_type_header;
reg  [7:0] mapper_header;
reg  [7:0] company_header;
reg  [3:0] rom_size;
reg [23:0] rom_mask, ram_mask;

wire [8:0] hdr_prefix = LHRom_type == 3 ? { 8'h40, 1'b1 } : // ExHiROM
                        LHRom_type == 2 ? { 8'h00, 1'b1 } : // HiROM
                        9'd0; // LoROM

always @(posedge clk_sys) begin
    reg [3:0] ram_size;

    if (cart_download) begin
        if(ioctl_wrD ^ ioctl_wr) begin
            if (ioctl_addr == 0) begin
                ram_size <= 4'h0;
                rom_type <= { 6'd0,
                  ( LHRom_type == 1 ? ( 2'b00 ) : // LoROM
                    LHRom_type == 2 ? ( 2'b01 ) : // HiROM
                    LHRom_type == 3 ? ( 2'b10 ) : // ExHiROM
                     ( 2'b11 )   // Auto
                  )
                };
            end

            if(LHRom_type == 0) //Auto, we will try to auto-detect
            begin

                if(ioctl_addr_adj == 25'h007FD4 ) mapper_header <= ioctl_dout[15:8]; // 0x20, 0x30 or 0x32
                if(ioctl_addr_adj == 25'h007FD6 ) { rom_size, rom_type_header } <= ioctl_dout[11:0];
                if(ioctl_addr_adj == 25'h007FD8 ) ram_size <= ioctl_dout[3:0];
                if(ioctl_addr_adj == 25'h007FDA ) company_header <= ioctl_dout[7:0];
                rom_type <= { 6'd0, 2'b00 }; // LoROM

                if (mapper_header != 8'h20 && mapper_header != 8'h30 && mapper_header != 8'h32 )
                begin
                    if(ioctl_addr_adj == 25'h00FFD4 ) mapper_header <= ioctl_dout[15:8]; // 0x21, 0x31, 0x35 or 0x25
                    if(ioctl_addr_adj == 25'h00FFD6 ) { rom_size, rom_type_header } <= ioctl_dout[11:0];
                    if(ioctl_addr_adj == 25'h00FFD8 ) ram_size <= ioctl_dout[3:0];
                    if(ioctl_addr_adj == 25'h00FFDA ) company_header <= ioctl_dout[7:0];
                    rom_type <= { 6'd0, 2'b01 }; // HiROM
                end

            end
            else
            begin //no auto-detection, some mode was selected on menu
                if(ioctl_addr_adj == { hdr_prefix, 15'h7FD4 }) mapper_header <= ioctl_dout[15:8];
                if(ioctl_addr_adj == { hdr_prefix, 15'h7FD6 }) { rom_size, rom_type_header } <= ioctl_dout[11:0];
                if(ioctl_addr_adj == { hdr_prefix, 15'h7FD8 }) ram_size <= ioctl_dout[3:0];
                if(ioctl_addr_adj == { hdr_prefix, 15'h7FDA }) company_header <= ioctl_dout[7:0];
            end

            rom_mask <= (24'd1024 << ((rom_size < 4'd7) ? 4'hC : rom_size)) - 1'd1;
            ram_mask <= ram_size ? (24'd1024 << ram_size) - 1'd1 : 24'd0;

        end
    end
    else begin
        PAL <= video_region;
        //DSP3
        if (mapper_header == 8'h30 && rom_type_header == 8'd5 && company_header == 8'hB2) rom_type[7:4] <= 4'hA;
        //DSP1
        else if (((mapper_header == 8'h20 || mapper_header == 8'h21) && rom_type_header == 8'd3) ||
            (mapper_header == 8'h30 && rom_type_header == 8'd5) ||
            (mapper_header == 8'h31 && (rom_type_header == 8'd3 || rom_type_header == 8'd5))) rom_type[7] <= 1'b1;
        //DSP2
        else if (mapper_header == 8'h20 && rom_type_header == 8'd5) rom_type[7:4] <= 4'h9;
        //DSP4
        else if (mapper_header == 8'h30 && rom_type_header == 8'd3) rom_type[7:4] <= 4'hB;
        //OBC1
        else if (mapper_header == 8'h30 && rom_type_header == 8'h25) rom_type[7:4] <= 4'hC;
        //SDD1
        else if (mapper_header == 8'h32 && (rom_type_header == 8'h43 || rom_type_header == 8'h45)) rom_type[7:4] <= 4'h5;
        //ST0XX
        else if (mapper_header == 8'h30 && rom_type_header == 8'hf6) begin
            rom_type[7:3] <= { 4'h8, 1'b1 };
            if (rom_size < 4'd10) rom_type[5] <= 1'b1; // Hayazashi Nidan Morita Shougi
        end
        //GSU
        else if (mapper_header == 8'h20 &&
            (rom_type_header == 8'h13 || rom_type_header == 8'h14 || rom_type_header == 8'h15 || rom_type_header == 8'h1a))
        begin
            rom_type[7:4] <= 4'h7;
            ram_mask <= (24'd1024 << 4'd6) - 1'd1;
        end
        //SA1
        else if (mapper_header == 8'h23 && (rom_type_header == 8'h32 || rom_type_header == 8'h34 || rom_type_header == 8'h35)) rom_type[7:4] <= 4'h6;
    end
end

////////////////////////////  SYSTEM  ///////////////////////////////////

main #(.USE_DSPn(1'b1), .USE_CX4(1'b0), .USE_SDD1(1'b0), .USE_SA1(1'b0), .USE_GSU(1'b0), .USE_DLH(1'b1), .USE_SPC7110(1'b0)) main
(
    .RESET_N(~reset),

    .MCLK(clk_sys), // 21.47727 / 21.28137
    .ACLK(clk_sys),
    .HALT(bk_state == 1'b1),

    .GSU_ACTIVE(),
    .GSU_TURBO(1'b0),

    .ROM_TYPE(rom_type),
    .ROM_MASK(rom_mask),
    .RAM_MASK(ram_mask),
    .PAL(PAL),
    .BLEND(BLEND),

    .ROM_ADDR(ROM_ADDR),
    .ROM_Q(ROM_Q),
    .ROM_CE_N(ROM_CE_N),
    .ROM_OE_N(ROM_OE_N),
    .ROM_WORD(ROM_WORD),

    .BSRAM_ADDR(BSRAM_ADDR),
    .BSRAM_D(BSRAM_D),
    .BSRAM_Q(BSRAM_Q),
    .BSRAM_CE_N(BSRAM_CE_N),
    .BSRAM_OE_N(BSRAM_OE_N),
    .BSRAM_WE_N(BSRAM_WE_N),
    .BSRAM_RD_N(BSRAM_RD_N),

    .WRAM_ADDR(WRAM_ADDR),
    .WRAM_D(WRAM_D),
    .WRAM_Q(WRAM_Q),
    .WRAM_CE_N(WRAM_CE_N),
    .WRAM_OE_N(WRAM_OE_N),
    .WRAM_WE_N(WRAM_WE_N),
    .WRAM_RD_N(WRAM_RD_N),

    .VRAM_OE_N(VRAM_OE_N),

    .VRAM1_ADDR(VRAM1_ADDR),
    .VRAM1_DI(VRAM1_Q),
    .VRAM1_DO(VRAM1_D),
    .VRAM1_WE_N(VRAM1_WE_N),

    .VRAM2_ADDR(VRAM2_ADDR),
    .VRAM2_DI(VRAM2_Q),
    .VRAM2_DO(VRAM2_D),
    .VRAM2_WE_N(VRAM2_WE_N),

    .ARAM_ADDR(ARAM_ADDR),
    .ARAM_D(ARAM_D),
    .ARAM_Q(ARAM_Q),
    .ARAM_CE_N(ARAM_CE_N),
    .ARAM_OE_N(ARAM_OE_N),
    .ARAM_WE_N(ARAM_WE_N),

    .R(R),
    .G(G),
    .B(B),

    .FIELD(),
    .INTERLACE(),
    .HIGH_RES(),
    .DOTCLK(DOTCLK),
    .DOT_CLK_CE(DOT_CLK_CE),

    .HBLANKn(HBLANKn),
    .VBLANKn(VBLANKn),
    .HSYNC(HSYNC),
    .VSYNC(VSYNC),

    .JOY1_DI(JOY1_DO),
    .JOY2_DI(GUN_MODE ? LG_DO : JOY2_DO),
    .JOY_STRB(JOY_STRB),
    .JOY1_CLK(JOY1_CLK),
    .JOY2_CLK(JOY2_CLK),
    .JOY1_P6(JOY1_P6),
    .JOY2_P6(JOY2_P6),
    .JOY2_P6_in(JOY2_P6_DI),

    .GG_EN(1'b0),
    .GG_CODE(),
    .GG_RESET(),
    .GG_AVAILABLE(1'b0),

    .TURBO(1'b0),
    .TURBO_ALLOW(),

    .AUDIO_L(audioL),
    .AUDIO_R(audioR)
);

////////////////////   VIDEO   //////////////////
//wire [7:0] R,G,B;
//wire       HSYNC,VSYNC;
//wire       HBLANKn,VBLANKn;
//wire       BLANK = ~(HBLANKn & VBLANKn);
//
//wire direct_video_s = ~status[9] ^ direct_video;
//wire [11:0] vga_col_s;
//wire vga_hs_s, vga_vs_s;
//
//framebuffer #(256,224,12,0) framebuffer
//(
//        .clk_sys    ( clk_sys ),
//        .clk_i      ( DOTCLK ),
//        .RGB_i      ((BLANK) ? 12'd0 : {R[7:4],G[7:4],B[7:4]} ),
//        .hblank_i   ( ~HBLANKn ),
//        .vblank_i   ( ~VBLANKn ),
//        .dis_db_i   ( 1'b0 ),
//        .rotate_i   ( 2'b00 ),
//
//        .clk_vga_i  ( clk_25m2 ), //640x480
//        .RGB_o      ( vga_col_s ),
//        .hsync_o    ( vga_hs_s ),
//        .vsync_o    ( vga_vs_s ),
//        .blank_o    (  ),
//
//        .odd_line_o (  )
//);
//
//mist_video #(.SD_HCNT_WIDTH(10), .COLOR_DEPTH(4), .USE_FRAMEBUFFER(1)) mist_video
//(
//
//    .scanlines(scanlines),
//    .scandoubler_disable(direct_video_s),
//    .rotate(2'b00),
//    .ce_divider(1'b0),
//    .SPI_DI(SPI_DI),
//    .SPI_SCK(SPI_SCK),
//    .SPI_SS3(SPI_SS2),
//
//    .clk_sys((direct_video_s) ? clk_sys : clk_25m2),
//    .HSync((direct_video_s) ? ~HSYNC : vga_hs_s),
//    .VSync((direct_video_s) ? ~VSYNC : vga_vs_s),
//    .R((direct_video_s) ? BLANK ? 4'd0 : ((LG_TARGET && GUN_MODE) ? {4{LG_TARGET[0]}} : R[7:4]) : vga_col_s[11:8]),
//    .G((direct_video_s) ? BLANK ? 4'd0 : ((LG_TARGET && GUN_MODE) ? {4{LG_TARGET[1]}} : G[7:4]) : vga_col_s[7:4]),
//    .B((direct_video_s) ? BLANK ? 4'd0 : ((LG_TARGET && GUN_MODE) ? {4{LG_TARGET[2]}} : B[7:4]) : vga_col_s[3:0]),
//    /*
//    .HSync(~HSYNC),
//    .VSync(~VSYNC),
//    .R(BLANK ? 6'd0 : ((LG_TARGET && GUN_MODE) ? {6{LG_TARGET[0]}} : R[7:2])),
//    .G(BLANK ? 6'd0 : ((LG_TARGET && GUN_MODE) ? {6{LG_TARGET[1]}} : G[7:2])),
//    .B(BLANK ? 6'd0 : ((LG_TARGET && GUN_MODE) ? {6{LG_TARGET[2]}} : B[7:2])),
//*/
//    .VGA_HS(VGA_HS),
//    .VGA_VS(VGA_VS),
//    .VGA_R          ( VGA_R          ),
//    .VGA_G          ( VGA_G          ),
//    .VGA_B          ( VGA_B          ),
//
//    .no_csync(~direct_video_s),
//    .osd_enable (osd_enable)
//);

//////////////////   VIDEO   //////////////////
wire [7:0] R,G,B;
wire       HSYNC,VSYNC;
wire       HBLANKn,VBLANKn;
wire       BLANK = ~(HBLANKn & VBLANKn);

// wire [5:0] vga_r_s; 
// wire [5:0] vga_g_s; 
// wire [5:0] vga_b_s; 

//wire direct_video;
wire direct_video_s = ~status[9] ^ direct_video;

mist_video #(.SD_HCNT_WIDTH(10), .COLOR_DEPTH(6), .USE_FRAMEBUFFER(0)) mist_video
(
    .clk_sys(clk_sys),
    .scanlines(scanlines),
    .scandoubler_disable(direct_video_s),
    .rotate(2'b00),
	 .blend(status[16]),
    .ce_divider(1'b0),
    .SPI_DI(SPI_DI),
    .SPI_SCK(SPI_SCK),
    .SPI_SS3(SPI_SS2),
    //.clk_sys((direct_video_s) ? clk_sys : clk_25m2),
    .HSync(~HSYNC),
    .VSync(~VSYNC),
    .R(BLANK ? 6'd0 : ((LG_TARGET && GUN_MODE) ? {6{LG_TARGET[0]}} : R[7:2])),
    .G(BLANK ? 6'd0 : ((LG_TARGET && GUN_MODE) ? {6{LG_TARGET[1]}} : G[7:2])),
    .B(BLANK ? 6'd0 : ((LG_TARGET && GUN_MODE) ? {6{LG_TARGET[2]}} : B[7:2])),

    .VGA_HS(VGA_HS),
    .VGA_VS(VGA_VS),
    .VGA_R          ( VGA_R          ),
    .VGA_G          ( VGA_G          ),
    .VGA_B          ( VGA_B          ),

    .no_csync(~direct_video_s),
    .osd_enable (osd_enable)
);

//////////////////   AUDIO   //////////////////
wire [15:0] audioL, audioR;

hybrid_pwm_sd dacl
(
    .clk(clk_sys),
    .n_reset(~reset),
    .din({~audioL[15], audioL[14:0]}),
    .dout(AUDIO_L)
);

hybrid_pwm_sd dacr
(
    .clk(clk_sys),
    .n_reset(~reset),
    .din({~audioR[15], audioR[14:0]}),
    .dout(AUDIO_R)
);

// I2S audio
wire MCLK;

audio_top audio_i2s
(
    .clk_50MHz ( clock_50_i ),
    .dac_MCLK  ( MCLK ),
    .dac_LRCK  ( LRCLK ),
    .dac_SCLK  ( SCLK ),
    .dac_SDIN  ( SDIN ),
    .L_data    ( audioL ),
    .R_data    ( audioR )
);	

////////////////////////////  I/O PORTS  ////////////////////////////////
wire [11:0] joy0;
wire [11:0] joy1;
wire [11:0] joy2 = { joystick2[7:6], joystick2[11:8], joystick2[5:0] };
wire [11:0] joy3 = { joystick3[7:6], joystick3[11:8], joystick3[5:0] };
wire [11:0] joy4 = { joystick4[7:6], joystick4[11:8], joystick4[5:0] };

   //-- joy_s format MXYZ SACB RLDU

assign joy0 = {
                m_fireD ,       //S - snes start
                m_fireH | ~btn_n_o[1], //M - Snes select
               ( joy1_mode ? m_fireB : m_fireE ), //c - snes R
               ( joy1_mode ? m_fireE : m_fireG ), //z - snes L
               ( joy1_mode ? m_fireG : m_fireC ), //x - snes Y
               ( joy1_mode ? m_fireF : m_fireF ), //y - snes x
               ( joy1_mode ? m_fireC : m_fireA ), //a - snes b
               ( joy1_mode ? m_fireA : m_fireB ), //b - snes a
                m_up    ,       // up
                m_down  ,       // down
                m_left  ,       // left
                m_right };      // right

assign joy1 = {
                m_fire2D,       //S - snes start
                m_fire2H| ~btn_n_o[2], //M - Snes select
               ( joy2_mode ? m_fire2B : m_fire2E ), //c - snes R
               ( joy2_mode ? m_fire2E : m_fire2G ), //z - snes L
               ( joy2_mode ? m_fire2G : m_fire2C ), //x - snes Y
               ( joy2_mode ? m_fire2F : m_fire2F ), //y - snes x
               ( joy2_mode ? m_fire2C : m_fire2A ), //a - snes b
               ( joy2_mode ? m_fire2A : m_fire2B ), //b - snes a
                m_up2,          // up
                m_down2,        // down
                m_left2,        // left
                m_right2 };     // right

wire       JOY_STRB;

wire [1:0] JOY1_DO;
wire       JOY1_CLK;
wire       JOY1_P6;
ioport port1
(
    .CLK(clk_sys),

    .PORT_LATCH(JOY_STRB),
    .PORT_CLK(JOY1_CLK),
    .PORT_P6(JOY1_P6),
    .PORT_DO(JOY1_DO),

    .JOYSTICK1(joy_swap ? joy1 : joy0),

    .MOUSE(ps2_mouse),
    .MOUSE_EN(mouse_mode[0])
);

wire [1:0] JOY2_DO;
wire       JOY2_CLK;
wire       JOY2_P6;
wire       JOY2_P6_DI = (LG_P6_out | !GUN_MODE);

ioport port2
(
    .CLK(clk_sys),

    .MULTITAP(multitap),

    .PORT_LATCH(JOY_STRB),
    .PORT_CLK(JOY2_CLK),
    .PORT_P6(JOY2_P6),
    .PORT_DO(JOY2_DO),

    .JOYSTICK1(joy_swap ? joy0 : joy1),
    .JOYSTICK2(joy2),
    .JOYSTICK3(joy3),
    .JOYSTICK4(joy4),

    .MOUSE(ps2_mouse),
    .MOUSE_EN(mouse_mode[1])
);

wire       LG_P6_out;
wire [1:0] LG_DO;
wire [2:0] LG_TARGET;
wire       LG_T = joy0[6] | joy1[6]; // always from joysticks
wire       DOTCLK;

lightgun lightgun
(
    .CLK(clk_sys),
    .RESET(reset),

    .MOUSE(ps2_mouse),
    .MOUSE_XY(1'b1),

    .JOY_X(),
    .JOY_Y(),

    .F(ps2_mouse[0]),
    .C(ps2_mouse[1]),
    .T(LG_T), // always from joysticks
    .P(ps2_mouse[2] | joy0[7] | joy1), // always from joysticks and mouse

    .HDE(HBLANKn),
    .VDE(VBLANKn),
    .CLKPIX(DOTCLK),

    .TARGET(LG_TARGET),
    .SIZE(1'b0),

    .PORT_LATCH(JOY_STRB),
    .PORT_CLK(JOY2_CLK),
    .PORT_P6(LG_P6_out),
    .PORT_DO(LG_DO)
);

//////////////////////////// BACKUP RAM /////////////////////
reg  [19:1] BSRAM_IO_ADDR;
wire [15:0] BSRAM_IO_D;
wire [15:0] BSRAM_IO_Q;
reg  [15:0] bsram_io_q_save;
reg         bsram_io_req;
reg         bk_ena, bk_load;
reg         bk_state;
reg  [11:0] sav_size;

wire bk_save;

assign      sd_buff_din = sd_buff_addr[0] ? bsram_io_q_save[15:8] : bsram_io_q_save[7:0];

always @(posedge clk_mem) begin

    reg img_mountedD;
    reg ioctl_downloadD;
    reg bk_loadD, bk_saveD;
    reg sd_ackD;
    reg [25:0] count_save = 25'd0;

    if (~bsram_wrD & bsram_wr & count_save == 25'd0)
    begin
        count_save <= 25'd63000000; // ~3 sec
    end

    if (count_save > 25'd0)
    begin
        count_save <= count_save - 25'd1;
        if (count_save < 25'd500 && count_save > 25'd100) bk_save <= 1'b1; else bk_save <= 1'b0;
    end

    if (reset) begin
        bk_ena <= 0;
        bk_state <= 0;
        bk_load <= 0;
    end else begin
        img_mountedD <= img_mounted;
        if (~img_mountedD & img_mounted) begin
            if (|img_size & ioctl_index == 8'h01) begin
                bk_ena <= 1;
                bk_load <= 1;
                sav_size <= img_size[20:9];
            end else begin
                bk_ena <= 0;
            end
        end

        ioctl_downloadD <= ioctl_download;
        if (~ioctl_downloadD & ioctl_download) bk_ena <= 0;

        bk_loadD <= bk_load;
        bk_saveD <= bk_save;
        sd_ackD  <= sd_ack;

        if (~sd_ackD & sd_ack) { sd_rd, sd_wr } <= 2'b00;

        case (bk_state)
        0:  if (bk_ena && ((~bk_loadD & bk_load) || (~bk_saveD & bk_save))) begin
                bk_state <= 1;
                sd_lba <= 0;
                sd_rd <= bk_load;
                sd_wr <= ~bk_load;
                if (bk_save) begin
                    BSRAM_IO_ADDR <= 0;
                    bsram_io_req <= ~bsram_io_req;
                end else
                    BSRAM_IO_ADDR <= 19'h7ffff;
            end
        1:  if (sd_ackD & ~sd_ack) begin
                if (sd_lba[11:0] == sav_size-1) begin
                    bk_load <= 0;
                    bk_state <= 0;
                end else begin
                    sd_lba <= sd_lba + 1'd1;
                    sd_rd  <= bk_load;
                    sd_wr  <= ~bk_load;
                end
            end
        endcase

        if (sd_buff_wr) begin
            if (sd_buff_addr[0]) begin
                BSRAM_IO_D[15:8] <= sd_buff_dout;
                bsram_io_req <= ~bsram_io_req;
                BSRAM_IO_ADDR <= BSRAM_IO_ADDR + 1'd1;
            end else
                BSRAM_IO_D[7:0] <= sd_buff_dout;
        end

        if (~sd_buff_addr[0]) bsram_io_q_save <= BSRAM_IO_Q;

        if (sd_buff_rd & sd_buff_addr[0]) begin
            bsram_io_req <= ~bsram_io_req;
            BSRAM_IO_ADDR <= BSRAM_IO_ADDR + 1'd1;
        end
    end
end


wire m_up, m_down, m_left, m_right, m_fireA, m_fireB, m_fireC, m_fireD, m_fireE, m_fireF, m_fireG, m_fireH;
wire m_up2, m_down2, m_left2, m_right2, m_fire2A, m_fire2B, m_fire2C, m_fire2D, m_fire2E, m_fire2F, m_fire2G, m_fire2H;
wire m_tilt, m_coin1, m_coin2, m_coin3, m_coin4, m_one_player, m_two_players, m_three_players, m_four_players;

wire m_right4, m_left4, m_down4, m_up4, m_right3, m_left3, m_down3, m_up3;

wire btn_one_player  = ~btn_n_o[1] | m_one_player;
wire btn_two_players = ~btn_n_o[2] | m_two_players;
wire btn_coin        = ~btn_n_o[3] | m_coin1;
wire [7:0] keys_s;
wire [15:0]joy1_s;
wire [15:0]joy2_s;
wire [8:0]controls_s;
wire osd_enable;
wire direct_video;
wire [1:0]osd_rotate;
wire [4:1]btn_n_o;

//translate scancode to joystick
MC2_HID #( .OSD_CMD( 3'b011 ), .USE_VKP( 1'b1), .CLK_SPEED(50000), .use_usb_g ( 1'b1 )) k_hid
(
    .clk_i          ( clock_50_i ),
    .kbd_clk_io     ( ps2_clk_io ),
    .kbd_dat_io     ( ps2_data_io ),
    .usb_rx_i       ( ps2_mouse_clk_io ),

    .joystick_0_i   ({ joy1_p9_i, joy1_p6_i, joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i }),
    .joystick_1_i   ({ joy2_p9_i, joy2_p6_i, joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i }),
      
    //-- joystick_0 and joystick_1 should be swapped
    .joyswap_i      ( 0 ),

    //-- player1 and player2 should get both joystick_0 and joystick_1
    .oneplayer_i    ( 0 ),

    //-- tilt, coin4-1, start4-1
    .controls_o     ( {m_tilt, m_coin4, m_coin3, m_coin2, m_coin1, m_four_players, m_three_players, m_two_players, m_one_player} ),

    //-- fire12-1, up, down, left, right

    .player1_o      ( {m_fireH, m_fireG,  m_fireF, m_fireE, m_fireD, m_fireC, m_fireB, m_fireA, m_up, m_down, m_left, m_right} ),
    .player2_o      ( {m_fire2H, m_fire2G, m_fire2F, m_fire2E, m_fire2D, m_fire2C, m_fire2B, m_fire2A, m_up2, m_down2, m_left2, m_right2} ),

    .direct_video_o ( direct_video ),
    .osd_rotate_o   ( osd_rotate ),

    //-- keys to the OSD
    .osd_o          ( keys_s ),
    .osd_enable_i   ( osd_enable ),
    
    //-- sega joystick
    .sega_strobe_o  ( joy_p7_o ),

     //-- Front buttons
     .front_buttons_i ( 4'b1111 ),
     .front_buttons_o ( btn_n_o )        
);




endmodule
