#!/usr/bin/env luajit
printf = function(s,...)
  return io.write(s:format(...))
end

 -- run without creating images, to test wthout image library and only cl.lua
if arg[1] ~= "noimg" then
require "gd"
end

ffi = require( "ffi" )
cl  = require( "cl" )
cl_error = nil
cl_error_buf = ffi.new( "int[1]" )

local function CODE(x)
   for c,r in ipairs(x) do
      for k,v in pairs(x) do
	 if type(k) == "string" then
	    r = r:gsub( "@"..k, v )
	 end
      end
      local fun, msg = loadstring( r )
      if fun == nil then
	 local col = msg:find(':')
	 msg = msg:sub(col)
	 error( "\nERROR" .. msg .. " IN SOURCE CODE:\n" .. x[c] .. "\nEXPANDED TO:\n" .. r )
      end
      fun()
   end
end

local function CHK(x)
   if x ~= cl.CL_SUCCESS then
      print("OpenCL error code:",x)
   end
--   print(x);
   assert(x == cl.CL_SUCCESS )
   return x
end

local source0 = [[
      __kernel void square(__global const float* input, __global float* output, const unsigned int count)
      {
	 int i = get_global_id(0);
	 if( i < count )
	     output[i] = input[i] * input[i];
      }
]]

-- float m_abs( float number ) {if ( number < 0 ) return 0-number; else return number;}

local cFuns = " ";
local sourceFun = [[
__kernel void square(__global const float* input, __global float* output, const unsigned int count)
{
	 float xabs, ord, abs2, ord2, old_x, bailchk;
	 int colorf=0;
	 int test=input[8];
	 float bailout=input[5];
	 int i = get_global_id(0);
	 if( i < count ) {
]]

local cVers = " ";
local sourceBeg = [[
	xabs=input[0]+input[2]*i; ord=input[1];
	do { colorf = colorf + 1;
]]

local BaseSqr = [[
	  abs2 = xabs*xabs; ord2 = ord*ord;
]]

local JuliaSqr = [[
	  old_x=xabs; xabs = abs2 - ord2 + input[3];
	  ord = input[7]*old_x*ord + input[4];
]]

local bailouttest = [[
	  if( colorf == input[6] ) bailchk=1;
	  else
 switch (test) {
  case 0: bailchk=abs2+ord2-bailout; break;
  case 1: bailchk=xabs*xabs-bailout; break;
  case 2: bailchk=ord*ord-bailout; break;
  case 3: abs2=xabs*xabs-bailout;
	 if (abs2 < 0) bailchk=ord*ord-bailout; else bailchk=abs2; break;
  case 4: abs2=xabs*xabs-bailout; ord2=ord*ord-bailout;
	 if (abs2 > 0 && ord2 > 0) bailchk=abs2; else bailchk=0; break;
  case 5: abs2=fabs(xabs)+fabs(ord); bailchk=abs2*abs2-bailout; break;
  case 6: abs2=xabs+ord; bailchk=abs2*abs2-bailout; break;
 }
]]

local sourceend = [[
	} while( bailchk <= 0 );
	 output[i] = colorf;
	 }
}
]]

local JuliaCube = [[
 xabs = xabs*(abs2 - input[7]*ord2) + input[3];
 ord = ord*(input[7]*abs2 - ord2) + input[4];
]]

local JuliaCuber = [[
 xabs = xabs*(abs2 - input[7]*ord2) + input[3];
 ord = ord*(ord2 - input[7]*abs2) + input[4];
]]

local BaseSin = [[
	  abs2 = cos(xabs*xabs); ord2 = sin(ord*ord);
]]

local BaseSina = [[
	  abs2 = acos(xabs*xabs); ord2 = asin(ord*ord);
]]

local BaseSinh = [[
	  abs2 = cosh(xabs); ord2 = sinh(ord);
]]

local BaseTan = [[
	  abs2 = 1/tan(xabs); ord2 = tan(ord);
]]

--	  xabs = sqrt((float)fabs(abs2 - ord2)) + input[3];
local JuliaSqrSqrt = [[  old_x=xabs;
	  xabs = abs2 - ord2; if ( xabs < 0 ) xabs=-xabs;
	  xabs = sqrt(xabs) + input[3];
	  ord = input[7]*old_x*ord + input[4];
]]

local JuliaSqrSqrta = [[  old_x=xabs;
	  xabs = sqrt(abs2 + ord2) + input[3];
	  ord = input[7]*old_x*ord + input[4];
]]

local BaseP = [[
	  abs2 = xabs*(xabs+input[7]); ord2 = ord*(ord+input[7]);
]]

local BaseCube = [[
	  abs2 = xabs*xabs*xabs; ord2 = ord*ord*ord;
]]

local C_Pow = [[
float2 c_pow(float2 x1, float2 x2) {
  float a,m,y,za;
  float2 cy;
  if ( x1.y == 0 ) {
    if ( x2.y == 0 ) {
	  cy.x=pow(x1.x,x2.x);
	  cy.y=0; return cy; }
    y=x1.x*x1.x;
    a=pow(y,(x2.x/2));
    m=log(y)*x2.y/2; }
  else if ( x2.y == 0 ) {
    a=pow((x1.x*x1.x+x1.y*x1.y),(x2.x/2));
    m=atan2(x1.y,x1.x)*x2.x; }
  else {
    za=atan2(x1.y,x1.x);
    y=x1.x*x1.x+x1.y*x1.y;
    a=pow(exp(-za*x2.y)*y,(x2.x/2));
    m=za*x2.x+log(y)*x2.y/2;
  }
  cy.x=a*cos(m); cy.y=a*sin(m); return cy;
}
]]

local C_Ver = [[
float2 cy,c1,c2,c3;
c2.x=input[3]; c2.y=input[4];
]]

local JuliaPow = [[ c1.x=xabs; c1.y=ord;
 cy=c_pow(c1, c2);
 xabs = cy.x; ord = cy.y;
]]

local C_VerP2 = [[ c3.x=input[7]; c3.y=0; ]]

local C_VerP3 = [[
 old_x=2.302585092994;
]]
--  old_x=log(10);

local JuliaPown = [[ c1.x=xabs; c1.y=ord;
 cy=c_pow(c1,c3);
 xabs = cy.x + c2.x; ord = cy.y + c2.y;
]]

local C_Log = [[
float loghypot(float r, float i) {
  float xr=fabs(r);
  float xi=fabs(i);
  float xd;
  if (xr==0) return log(xi);
  if (xi==0) return log(xr);
  if (xr>xi) { xd=xi/xr; return log(xr)+log(1+xd*xd)/2; }
  xd=xr/xi; return log(xi)+log(1+xd*xd)/2;
}

float2 c_log(float2 xp) {
  float2 cy;
  if ( xp.y == 0 ) { cy.x=log(xp.x); cy.y=0; return cy; }
  cy.x=loghypot(xp.x,xp.y);
  cy.y=atan2(xp.y,xp.x);
  return cy;
}
]]

local JuliaLog = [[ c1.x=xabs; c1.y=ord;
 cy=c_log(c1); cy=c_pow(cy, c3);
 xabs = cy.x + c2.x; ord = cy.y + c2.y;
]]

local JuliaLog10 = [[ c1.x=xabs; c1.y=ord;
 cy=c_log(c1); c1.x=cy.x/old_x; c1.y=cy.y/old_x; cy=c_pow(c1, c3);
 xabs = cy.x + c2.x; ord = cy.y + c2.y;
]]

local C_Exp = [[
float2 c_exp(float2 xp) {
  float2 cy;
  float ce;
  ce=exp(xp.x);
  if ( xp.y == 0 ) { cy.x=ce; cy.y=0; return cy; }
  cy.x=ce*cos(xp.y); cy.y=ce*sin(xp.y);
  return cy;
}
]]

local JuliaExp = [[ c1.x=xabs; c1.y=ord;
 cy=c_exp(c1); cy=c_pow(cy, c3);
 xabs = cy.x + c2.x; ord = cy.y + c2.y;
]]

local C_Sin = [[
float2 c_sin(float2 xp) {
  float2 cy;
  float e1, e2;
  if ( xp.y == 0 ) { cy.x=sin(xp.x); cy.y=0; return cy; }
  e1=exp(-xp.y); e2=exp(xp.y);
  cy.x=(e1+e2)*sin(xp.x)/2;
  cy.y=(e2-e1)*cos(xp.x)/2;
  return cy;
}
]]

local JuliaSin = [[ c1.x=xabs; c1.y=ord;
 cy=c_sin(c1); cy=c_pow(cy, c3);
 xabs = cy.x + c2.x; ord = cy.y + c2.y;
]]

--  if( test == 0 ) bailchk=abs2+ord2-bailout;
--  else if (test == 1) bailchk=xabs*xabs-bailout;
--  else if (test == 2) bailchk=ord*ord-bailout;
--  else if (test == 3) { abs2=xabs*xabs-bailout;
--	 if (abs2 < 0) bailchk=ord*ord-bailout; else bailchk=abs2; }
--  else if (test == 4) { abs2=xabs*xabs-bailout; ord2=ord*ord-bailout;
--	 if (abs2 > 0 && ord2 > 0) bailchk=abs2; else bailchk=0; }
--  else if (test == 5) bailchk=((abs(xabs) + abs(ord))^2-bailout);
--  else if (test == 6) bailchk=((xabs + ord)^2-bailout);

function clEnqueueNDRangeKernel(queue, kernel, global_offsets, global_sizes, local_sizes)
   local dim = #global_sizes
   if (global_offsets ~= nil and dim ~= #global_offsets) or dim ~= #local_sizes then
      error( "clEnqueueNDRangeKernel: global_sizes must have the same dimension of local_sizes and global_offsets, unless global_offsets is nil" )
   end
   local gwo
   if global_offsets ~= nil then
      gwo = ffi.new( "size_t[?]", dim, global_offsets )
   end
   local gws = ffi.new( "size_t[?]", dim, global_sizes )
   local lws = ffi.new( "size_t[?]", dim, local_sizes )
   local err = cl.clEnqueueNDRangeKernel(queue, kernel, dim, gwo, gws, lws, 0, nil, nil)
   if err ~= cl.CL_SUCCESS then
      error( "clEnqueueNDRangeKernel: " .. err )
   end
end

function clBuildProgram(program, devices, options)
   local err = ffi.new( "int[1]" )
   local kernel = cl.clBuildProgram(program, n_dev, dev, options, nil, nil)
   if err[0] ~= cl.CL_SUCCESS then
      error( "clBuildProgram: " .. err[0] )
   end
   return kernel
end

function defcl1(name, args, callargs)
   CODE {
      name = name,
      args = args,
      callargs = callargs or args,
      [[
	    function @name(@args)
--	      /* print(cl) */
	       cl_error = cl.@name(@callargs)
	       if cl_error ~= cl.CL_SUCCESS then
		  error( "@name: " .. cl_error )
	       end
	    end
      ]]
   }
end

function defcl2(name, args, callargs)
   CODE {
      name = name,
      args = args,
      callargs = callargs or args,
      [[
	    function @name(@args)
	       local obj = cl.@name(@callargs, cl_error_buf)
	       cl_error = cl_error_buf[0]
	       if cl_error ~= cl.CL_SUCCESS then
		  error( "@name: " .. cl_error )
	       end
	       return obj
	    end
      ]]
   }
end

local function wrap1(func, args)
 local err;
   _G[func] = function()
		if args then err = cl[func](args);
		  if err ~= cl.CL_SUCCESS then error( func .. ": " .. err ) end
		else err = cl[func]() end
	end
end

local function dump( value )
  if type(value) ~= "table" then
     return tostring(value)
  end
  local s = "{ "
  local c = ""
  for k,v in pairs(value) do
    s = s .. c .. dump(k) .. " = " .. dump(v)
    c = ", "
  end
  if getmetatable(value) then
  for k,v in pairs(getmetatable(value)) do
    s = s .. c .. dump(k) .. " = " .. dump(v)
    c = ", "
  end
  end
  s = s .. " }"
  return s;
end


-- cl.clGetPlatformIDS         -> clGetPlatforms
-- cl.clGetPlatformInfo        -> clGetPlatforms
-- cl.clGetDeviceIDs           -> clGetDevices
-- cl.clGetDeviceInfo          -> clGetDevices
-- cl.clCreateContext          -> clCreateContext
defcl1( "clRetainContext",       "cl_context" )
defcl1( "clReleaseContext",      "cl_context" )
defcl1( "clFinish",              "queue"  )
defcl1( "clReleaseMemObject",    "input"  )
defcl1( "clReleaseProgram",      "program"  )
defcl1( "clReleaseKernel",       "kernel"  )
defcl1( "clReleaseCommandQueue", "queue"   )
defcl1( "clReleaseContext",      "context" )
defcl1( "clSetKernelArg",        "kernel, index, data", "kernel, index, ffi.sizeof(data), data" )

defcl2( "clCreateKernel",        "program, kernel_name" )
defcl2( "clCreateBuffer",        "context, type, size", "context, type, size, nil" )


wrap1( "clRetainContext", "cl_context" )
wrap1( "clUnloadCompiler", nil )

clUnloadCompiler()

--[[]]
fractN=2;

local function clset(devidx)
	local timeb=os.time();
   platform_devices = {}
   devices = {}
   for platform_index, platform in pairs(clGetPlatforms()) do
      platform_devices = clGetDevices(platform.id)
      for device_index, device in pairs(platform_devices) do
	 devices[ #devices + 1 ] = device
      end
   end

   ffi_devices = ffi.new( "cl_device_id[?]", #devices )
   for k, _ in ipairs( devices ) do
      ffi_devices[k] = devices[ k ].id
--      print( devices[k].id )
   end

--   devidx = 0
--   print( "You chose device index " .. devidx .. " of [0.." .. #devices - 1 .. "]" )
   if devidx < 0 or devidx >= #devices then
     dump(platform_devices); print("devices #" .. #platform_devices);
      error( "ERROR: ".. #devices .. " devices found. Choose from 0.." .. #devices - 1 .. "!" )
   end
   ffi_device = ffi.new( "cl_device_id[1]", ffi_devices[devidx+1] )
-- print(dump(devices[devidx+1]));
-- print(dump(devices[devidx+1].platform));
   local err = ffi.new( "int[1]" )
   context_properties = ffi.new( "cl_context_properties[3]", cl.CL_CONTEXT_PLATFORM, ffi.cast("intptr_t",devices[devidx+1].platform), ffi.cast("intptr_t", nil) )
--   local context = cl.clCreateContext(context_properties, 1, ffi_device, nil, nil, err)
   context = cl.clCreateContextFromType(context_properties, cl.CL_DEVICE_TYPE_GPU, nil, nil, err)

--   if context then print( "CONTEXT ", context ); print(err[1]) end
if err[0] ~= cl.CL_SUCCESS then
   print(err[0]);
   assert(context ~= nil and CHK(err[0]))
end
   context_info = clGetContextInfo(context)

--   print( "INFO ", context_info )
end

local function demo1( devidx, fractal, imagename, ix, iy )
  local count = 1024
  local err = ffi.new( "int[1]" )
	local timeb=os.time();

   local commands = cl.clCreateCommandQueue(context, devices[devidx+1].id, 0, err)
   assert(CHK(err[0]) and commands ~= nil)

--   print( "COMMANDS ", commands )
   local source = cFuns .. sourceFun .. cVers .. sourceBeg .. fractal .. bailouttest .. sourceend;
   local src = ffi.new("char[?]", #source+1, source)
   local src2 = ffi.new("const char*[1]", src)
   local program = cl.clCreateProgramWithSource(context, 1, src2, nil, err)
   if not program or err[0] ~= cl.CL_SUCCESS then io.write(source); end
--   print(cVers);
   assert(CHK(err[0]) and program ~= nil)

   CHK(cl.clBuildProgram(program, 0, nil, nil, nil, nil))

   local kernel = cl.clCreateKernel(program, "square", err)
   assert(CHK(err[0]) and kernel ~= nil)

--   print( "KERNEL ", kernel )

   local data = ffi.new("float[?]", count)
   for i=0,count-1 do data[i] = (i + 10) end
   data[0]=-1.5; data[1]=-1.25; data[2]=3/count;
   local yh=math.floor(count*0.75); dy=2.5/yh;
   data[3]=ix; data[4]=iy;
   data[5]=4; data[6]=121; data[7]=fractN; data[8]=4;

--   print( "DATA ", data )

   local input  = clCreateBuffer(context, cl.CL_MEM_READ_ONLY,  ffi.sizeof(data))
   local output = clCreateBuffer(context, cl.CL_MEM_WRITE_ONLY, ffi.sizeof(data))

   CHK(cl.clEnqueueWriteBuffer(commands, input, cl.CL_TRUE, 0, ffi.sizeof(data), data, 0, nil, nil))

--   print( "ENQUEUED? " )

   local input2 = ffi.new("cl_mem[1]", input)
   clSetKernelArg(kernel, 0, input2)

   local output2 = ffi.new("cl_mem[1]", output)
   clSetKernelArg(kernel, 1, output2)

   local count2 = ffi.new("int[1]", count)
   clSetKernelArg(kernel, 2, count2)

   local ff_device = ffi.new( "cl_device_id[2]", context_info.devices )
-- print("devices:" .. dump(devices[devidx+1]));
--   print("context_info.properties:" .. dump(context_info.properties));
   local work_group_info = clGetKernelWorkGroupInfo( kernel, devices[devidx+1].id); -- ffi_device); -- ffi_devices[devidx+1]); )
-- print(dump(work_group_info));
   local work_group_size = math.floor(devices[devidx+1].max_work_group_size/2); -- 512; -- work_group_info.work_group_size
	print(imagename .. " build time = " .. os.time()-timeb);
   local results = ffi.new("float[?]", count)
   printf(" mem buffers: " .. (ffi.sizeof(results)+ffi.sizeof(input)+ffi.sizeof(output)) .. " " )

   local red, green, blue, maxcolor, c16, red2, crange;
   local color={}
   c16=256*256; maxcolor=256*c16; crange=math.floor(maxcolor/(data[6]*3));
--   print(count .. "," .. yh);
   if arg[1] ~= "noimg" then
    Im = gd.createTrueColor(count, yh);
   end
   color[1]=crange*11+85; -- printf("color1,");
   for i=2,data[6],1 do color[i]=color[i-1]+crange;
	 green=math.floor(color[i]/c16); red2=color[i]-(green*c16);
	 red=math.floor(red2/256); blue=red2-(red*256);
    if arg[1] ~= "noimg" then
     color[i]=Im:colorAllocate(red, green, blue); -- printf(i .. ",");
    end
   end

   timeb=os.time();
   for j=0,yh-1,1 do -- printf(j .. " ");
	data[1]=-1.25+j*dy;
   CHK(cl.clEnqueueWriteBuffer(commands, input, cl.CL_TRUE, 0, ffi.sizeof(data), data, 0, nil, nil))
   clEnqueueNDRangeKernel( commands, kernel, nil, { count }, {work_group_size}) -- work_group_info.work_group_size )
   clFinish( commands )
   CHK(cl.clEnqueueReadBuffer(commands, output, cl.CL_TRUE, 0, ffi.sizeof(results), results, 0, nil, nil))
    for i=0,count-1 do
	 if results[i] >= 1 and results[i] <= data[6] then
    if arg[1] ~= "noimg" then
     Im:setPixel(i+1, j+1, color[results[i]]);
    end
	 else print("error: " .. results[i]); end
    end
   end
--   print(".");
	local timec=os.time()-timeb;
--   for i=0,count-1 do io.stdout:write(i .. ": " .. results[i] .. '\t') end
--   io.stdout:write("\n")
	print("run time = " .. timec);
  if arg[1] ~= "noimg" then
 	 for i=1,data[6],1 do Im:setPixel(i,1,color[i]); end
    Im:png( imagename );
  end

  if arg[2] == "quit" then
   return;
  end
  
   clReleaseMemObject(input);
   clReleaseMemObject(output);
   clReleaseProgram(program);
   clReleaseKernel(kernel);
   clReleaseCommandQueue(commands);
   -- clReleaseContext(context);


--   print( "END!" )
	 local abs, ord, abs2, ord2, old_x;
	 local colorf=0;
   for i=0,count-count-1 do
	abs=data[0]+data[2]*i; ord=data[1]; colorf=0;
	repeat
	  old_x=abs; colorf = colorf + 1;
	  abs2 = abs*abs; ord2 = ord*ord;
	  abs = abs2 - ord2 + data[3];
	  ord = data[7]*old_x*ord + data[4];
	until ( (abs2+ord2)>=data[5] or colorf == data[6] );
	 results[i] = colorf;
   end

--   for i=0,count-1 do io.stdout:write(i .. ": " .. results[i] .. '\t') end
end

--demo1(true)
local ix=10/27; local iy=5/42;
cFuns=C_Pow .. C_Log; cVers=C_Ver .. C_VerP2 .. C_VerP3;
ix=-5/6; iy=5/27; fractN=9;
clset(0)
demo1(0, JuliaLog10, "spirl01.png", ix, iy);

if not arg[2] or arg[2] ~= "quit" then

cVers=C_Ver .. C_VerP2;
ix=-10/13; iy=1/12; fractN=-5;
demo1(0, JuliaLog, "spirl02.png", ix, iy);
ix=-5/7; iy=5/17; fractN=-2;
demo1(0, JuliaLog, "spirl03.png", ix, iy);
ix=-5/43; iy=10/11; fractN=1;
demo1(0, JuliaLog, "spirl04.png", ix, iy);
ix = -10/7; iy = 10/123; fractN=2;
demo1(0, JuliaLog, "spirl05.png", ix, iy);
ix = 5/83 - 1/6; iy = 10/33 + 1/172; fractN=3;
demo1(0, JuliaLog, "spirl06.png", ix, iy);
ix=5/43; iy=1/3; fractN=4;
demo1(0, JuliaLog, "spirl07.png", ix, iy);
ix=10/59; iy=10/57; fractN=5;
demo1(0, JuliaLog, "spirl08.png", ix, iy);
ix = -100/89; iy = 5/63; fractN=6;
demo1(0, JuliaLog, "spirl09.png", ix, iy);
ix = -10/60; iy = 10/179; fractN=7;
demo1(0, JuliaLog, "spirl10.png", ix, iy);
ix = -4/25; iy = 1/18; fractN=8; cFuns=C_Pow .. C_Exp;
demo1(0, JuliaExp, "spir001.png", ix, iy);
cFuns=C_Pow; cVers=C_Ver .. C_VerP2; fractN=2;
ix=-1.25; iy=5/29;
demo1(0, JuliaPown, "spir002.png", ix, iy);
fractN=3; ix=0.1; iy=5/8; -- ix = 5/53; iy = 1;
demo1(0, JuliaPown, "spir003.png", ix, iy);
fractN=4; ix = 5/52; iy = 5/6;
demo1(0, JuliaPown, "spir004.png", ix, iy);
fractN=5; ix=5/14; iy=10/57; -- ix = 5/16 + 17/1580; iy = 5/21 + 93/800;
demo1(0, JuliaPown, "spir005.png", ix, iy);
fractN=6; ix = 2/21; iy = 10/11;
demo1(0, JuliaPown, "spir006.png", ix, iy);
cFuns=" "; cVers=" "; fractN=2;

ix = 2/85 + 1/93 + 1/166; iy = -1/12 - 3/4;
-- ix = -1; iy = 5/63;
demo1(0, BaseSqr .. JuliaSqr, "spir1.png", ix, iy);
demo1(0, BaseSqr .. JuliaCube, "spir2.png", ix, iy);
demo1(0, BaseSin .. JuliaSqr, "spir3.png", ix, iy);
demo1(0, BaseSina .. JuliaSqr, "spir4.png", ix, iy);
demo1(0, BaseSinh .. JuliaSqr, "spir5.png", ix, iy);
demo1(0, BaseTan .. JuliaSqr, "spir6.png", ix, iy);
demo1(0, BaseSqr .. JuliaSqrSqrt, "spir7.png", ix, iy);
demo1(0, BaseSqr .. JuliaSqrSqrta, "spir8.png", ix, iy);
demo1(0, BaseP .. JuliaSqr, "spir9.png", ix, iy);
demo1(0, BaseP .. JuliaCube, "spir10.png", ix, iy);
demo1(0, BaseCube .. JuliaSqr, "spir11.png", ix, iy);
demo1(0, BaseCube .. JuliaCube, "spir12.png", ix, iy);
demo1(0, BaseSin .. JuliaCube, "spir13.png", ix, iy);
demo1(0, BaseSina .. JuliaCube, "spir14.png", ix, iy);
demo1(0, BaseSinh .. JuliaCube, "spir15.png", ix, iy);
demo1(0, BaseTan .. JuliaCube, "spir16.png", ix, iy);
demo1(0, BaseCube .. JuliaSqrSqrt, "spir17.png", ix, iy);
demo1(0, BaseCube .. JuliaSqrSqrta, "spir18.png", ix, iy);
demo1(0, BaseP .. JuliaSqrSqrt, "spir19.png", ix, iy);
demo1(0, BaseP .. JuliaSqrSqrta, "spir20.png", ix, iy);
demo1(0, BaseSin .. JuliaSqrSqrt, "spir21.png", ix, iy);
demo1(0, BaseSina .. JuliaSqrSqrt, "spir22.png", ix, iy);
demo1(0, BaseSinh .. JuliaSqrSqrt, "spir23.png", ix, iy);
demo1(0, BaseTan .. JuliaSqrSqrt, "spir24.png", ix, iy);
demo1(0, BaseSin .. JuliaSqrSqrta, "spir25.png", ix, iy);
demo1(0, BaseSina .. JuliaSqrSqrta, "spir26.png", ix, iy);
demo1(0, BaseSinh .. JuliaSqrSqrta, "spir27.png", ix, iy);
demo1(0, BaseTan .. JuliaSqrSqrta, "spir28.png", ix, iy);
demo1(0, BaseSqr .. JuliaCuber, "spir29.png", ix, iy);
demo1(0, BaseCube .. JuliaCuber, "spir30.png", ix, iy);
demo1(0, BaseSin .. JuliaCuber, "spir31.png", ix, iy);
demo1(0, BaseSina .. JuliaCuber, "spir32.png", ix, iy);
demo1(0, BaseSinh .. JuliaCuber, "spir33.png", ix, iy);
demo1(0, BaseTan .. JuliaCuber, "spir34.png", ix, iy);
fractN=3;
demo1(0, BaseSqr .. JuliaSqr, "spir35.png", ix, iy);
demo1(0, BaseSqr .. JuliaCube, "spir36.png", ix, iy);
demo1(0, BaseSin .. JuliaSqr, "spir37.png", ix, iy);
demo1(0, BaseSina .. JuliaSqr, "spir38.png", ix, iy);
demo1(0, BaseSinh .. JuliaSqr, "spir39.png", ix, iy);
demo1(0, BaseTan .. JuliaSqr, "spir40.png", ix, iy);
demo1(0, BaseSqr .. JuliaSqrSqrt, "spir41.png", ix, iy);
demo1(0, BaseSqr .. JuliaSqrSqrta, "spir42.png", ix, iy);
demo1(0, BaseP .. JuliaSqr, "spir43.png", ix, iy);
demo1(0, BaseP .. JuliaCube, "spir44.png", ix, iy);
demo1(0, BaseCube .. JuliaSqr, "spir45.png", ix, iy);
demo1(0, BaseCube .. JuliaCube, "spir46.png", ix, iy);
demo1(0, BaseSin .. JuliaCube, "spir47.png", ix, iy);
demo1(0, BaseSina .. JuliaCube, "spir48.png", ix, iy);
demo1(0, BaseSinh .. JuliaCube, "spir49.png", ix, iy);
demo1(0, BaseTan .. JuliaCube, "spir50.png", ix, iy);

clset(1);
fractN=2;
demo1(1, BaseSqr .. JuliaSqr, "spir1.png", ix, iy);
demo1(1, BaseSqr .. JuliaCube, "spir2.png", ix, iy);
demo1(1, BaseSin .. JuliaSqr, "spir3.png", ix, iy);
demo1(1, BaseSina .. JuliaSqr, "spir4.png", ix, iy);
demo1(1, BaseSinh .. JuliaSqr, "spir5.png", ix, iy);
demo1(1, BaseTan .. JuliaSqr, "spir6.png", ix, iy);
demo1(1, BaseSqr .. JuliaSqrSqrt, "spir7.png", ix, iy);
demo1(1, BaseSqr .. JuliaSqrSqrta, "spir8.png", ix, iy);
demo1(1, BaseP .. JuliaSqr, "spir9.png", ix, iy);
demo1(1, BaseP .. JuliaCube, "spir10.png", ix, iy);
demo1(1, BaseCube .. JuliaSqr, "spir11.png", ix, iy);
demo1(1, BaseCube .. JuliaCube, "spir12.png", ix, iy);
demo1(1, BaseSin .. JuliaCube, "spir13.png", ix, iy);
demo1(1, BaseSina .. JuliaCube, "spir14.png", ix, iy);
demo1(1, BaseSinh .. JuliaCube, "spir15.png", ix, iy);
demo1(1, BaseTan .. JuliaCube, "spir16.png", ix, iy);
demo1(1, BaseCube .. JuliaSqrSqrt, "spir17.png", ix, iy);
demo1(1, BaseCube .. JuliaSqrSqrta, "spir18.png", ix, iy);
demo1(1, BaseP .. JuliaSqrSqrt, "spir19.png", ix, iy);
demo1(1, BaseP .. JuliaSqrSqrta, "spir20.png", ix, iy);
demo1(1, BaseSin .. JuliaSqrSqrt, "spir21.png", ix, iy);
demo1(1, BaseSina .. JuliaSqrSqrt, "spir22.png", ix, iy);
demo1(1, BaseSinh .. JuliaSqrSqrt, "spir23.png", ix, iy);
demo1(1, BaseTan .. JuliaSqrSqrt, "spir24.png", ix, iy);
demo1(1, BaseSin .. JuliaSqrSqrta, "spir25.png", ix, iy);
demo1(1, BaseSina .. JuliaSqrSqrta, "spir26.png", ix, iy);
demo1(1, BaseSinh .. JuliaSqrSqrta, "spir27.png", ix, iy);
demo1(1, BaseTan .. JuliaSqrSqrta, "spir28.png", ix, iy);
demo1(1, BaseSqr .. JuliaCuber, "spir29.png", ix, iy);
demo1(1, BaseCube .. JuliaCuber, "spir30.png", ix, iy);
demo1(1, BaseSin .. JuliaCuber, "spir31.png", ix, iy);
demo1(1, BaseSina .. JuliaCuber, "spir32.png", ix, iy);
demo1(1, BaseSinh .. JuliaCuber, "spir33.png", ix, iy);
demo1(1, BaseTan .. JuliaCuber, "spir34.png", ix, iy);
fractN=3;
demo1(1, BaseSqr .. JuliaSqr, "spir35.png", ix, iy);
demo1(1, BaseSqr .. JuliaCube, "spir36.png", ix, iy);
demo1(1, BaseSin .. JuliaSqr, "spir37.png", ix, iy);
demo1(1, BaseSina .. JuliaSqr, "spir38.png", ix, iy);
demo1(1, BaseSinh .. JuliaSqr, "spir39.png", ix, iy);
demo1(1, BaseTan .. JuliaSqr, "spir40.png", ix, iy);
demo1(1, BaseSqr .. JuliaSqrSqrt, "spir41.png", ix, iy);
demo1(1, BaseSqr .. JuliaSqrSqrta, "spir42.png", ix, iy);
demo1(1, BaseP .. JuliaSqr, "spir43.png", ix, iy);
demo1(1, BaseP .. JuliaCube, "spir44.png", ix, iy);
demo1(1, BaseCube .. JuliaSqr, "spir45.png", ix, iy);
demo1(1, BaseCube .. JuliaCube, "spir46.png", ix, iy);
demo1(1, BaseSin .. JuliaCube, "spir47.png", ix, iy);
demo1(1, BaseSina .. JuliaCube, "spir48.png", ix, iy);
demo1(1, BaseSinh .. JuliaCube, "spir49.png", ix, iy);
demo1(1, BaseTan .. JuliaCube, "spir50.png", ix, iy);
end
--[[]]
