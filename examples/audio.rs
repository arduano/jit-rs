use jit_rs::codegen::LlvmCodegen;
use jit_rs::macro_builder::*;
use jit_rs::mir::mir_parse_module;
use jit_rs::tree_parser::*;
use macros::jit_quote;

fn main() {
    let width = 8usize;

    let tokens = jit_quote! {
        const WIDTH = #width;

        struct EnvelopeLinearTime {
            time_simd: <f32; WIDTH>,
            end_time: f32,
        }

        fn get_env_progress(env: *EnvelopeLinearTime) -> f32 {
            env.time_simd[0usize] / env.end_time
        }

        fn get_env_progress_vec(env: *EnvelopeLinearTime) -> <f32; WIDTH> {
            env.time_simd / env.end_time
        }

        fn increment_env_time_by(env: *EnvelopeLinearTime, by: u32) {
            env.time_simd = env.time_simd + by as f32;
        }

        fn make_env_time(end: u32) -> EnvelopeLinearTime {
            let vec = zeroed::<<f32; WIDTH>>();

            let i = 0usize;
            while i < WIDTH {
                vec[i] = i as f32;
                i = i + 1usize;
            }

            EnvelopeLinearTime {
                time_simd: vec,
                end_time: end as f32,
            }
        }

        struct EnvelopeParam {
            target: f32,
            duration: u32,
            kind: u8,
        }

        struct EnvelopeParams {
            stages: [EnvelopeParam; 7usize],
        }

        pub fn make_envelope_params() -> EnvelopeParams {
            let params = zeroed::<EnvelopeParams>();
            // Delay
            params.stages[0usize] = EnvelopeParam {
                target: 0.0f32,
                duration: 10u32,
                kind: 0u8,
            };
            // Attack
            params.stages[1usize] = EnvelopeParam {
                target: 1.0f32,
                duration: 10u32,
                kind: 1u8,
            };
            // Hold
            params.stages[2usize] = EnvelopeParam {
                target: 1.0f32,
                duration: 10u32,
                kind: 0u8,
            };
            // Decay
            params.stages[3usize] = EnvelopeParam {
                target: 0.7f32,
                duration: 10u32,
                kind: 1u8,
            };
            // Sustain
            params.stages[4usize] = EnvelopeParam {
                target: 0.7f32,
                duration: 10u32,
                kind: 0u8,
            };
            // Release
            params.stages[5usize] = EnvelopeParam {
                target: 0.0f32,
                duration: 10u32,
                kind: 1u8,
            };
            // Finished
            params.stages[6usize] = EnvelopeParam {
                target: 0.0f32,
                duration: 4294967293u32,
                kind: 0u8,
            };

            return params;
        }

        struct EnvelopeStage {
            linear_time: EnvelopeLinearTime,
            hold_time: u32,

            start_value: f32,
            end_value_offset: f32,
            end_value: f32,

            kind: u8,
            // 0 = hold
            // 1 = linear
        }

        fn make_stage_from_param(params: EnvelopeParam, start_val: f32) -> EnvelopeStage {
            EnvelopeStage {
                linear_time: make_env_time(params.duration),
                hold_time: params.duration,
                start_value: start_val,
                end_value_offset: params.target - start_val,
                end_value: params.target,
                kind: params.kind,
            }
        }

        fn sample_stage_vec(stage: *EnvelopeStage) -> <f32; WIDTH> {
            if stage.kind == 0u8 {
                stage.hold_time = stage.hold_time - 8u32;
                return extend::<<f32; WIDTH>>(stage.end_value);
            } else if stage.kind == 1u8 {
                let time = get_env_progress_vec(&stage.linear_time);
                increment_env_time_by(&stage.linear_time, 8u32);

                let val = stage.start_value + time * stage.end_value_offset;
                return val;
            } else {
                unreachable();
            }
        }

        fn sample_stage_unit(stage: *EnvelopeStage) -> f32 {
            if stage.kind == 0u8 {
                stage.hold_time = stage.hold_time - 1u32;
                return stage.end_value;
            } else if stage.kind == 1u8 {
                let time = get_env_progress(&stage.linear_time);
                increment_env_time_by(&stage.linear_time, 1u32);

                let val = stage.start_value + time * stage.end_value_offset;
                return val;
            } else {
                unreachable();
            }
        }

        fn is_stage_ending_in_next_vec(stage: *EnvelopeStage) -> bool {
            if stage.kind == 0u8 {
                return stage.hold_time < 8u32;
            } else if stage.kind == 1u8 {
                return stage.linear_time.time_simd[WIDTH - 1usize] >= stage.linear_time.end_time;
            } else {
                unreachable();
            }
        }

        fn samples_until_stage_end(stage: *EnvelopeStage) -> u32 {
            if stage.kind == 0u8 {
                return stage.hold_time;
            } else if stage.kind == 1u8 {
                let time = &stage.linear_time;
                let remaining_f32 = time.end_time - time.time_simd[0usize];
                return remaining_f32 as u32;
            } else {
                unreachable();
            }
        }

        struct Envelope {
            params: EnvelopeParams,
            stage: u8,
            stage_data: EnvelopeStage,
            last_val: f32,
        }

        fn make_envelope(params: EnvelopeParams) -> Envelope {
            let first_stage = params.stages[0usize];
            Envelope {
                params: params,
                stage: 0u8,
                stage_data: make_stage_from_param(first_stage, 0.0f32),
                last_val: 0.0f32,
            }
        }

        fn sample_envelope_vec_manual(env: *Envelope) -> <f32; WIDTH> {
            let val = extend::<<f32; WIDTH>>(0.0f32);

            let i = 0usize;
            while i < WIDTH {
                let until_end = samples_until_stage_end(&env.stage_data) as usize;
                if until_end <= 0usize {
                    env.stage = env.stage + 1u8;
                    let next_stage = env.params.stages[env.stage as usize];
                    env.stage_data = make_stage_from_param(next_stage, env.last_val);
                }

                let next = sample_stage_unit(&env.stage_data);
                val[i] = next;
                env.last_val = next;

                i = i + 1usize;
            }

            return val;
        }

        fn sample_envelope_vec(env: *Envelope) -> <f32; WIDTH> {
            let until_end = samples_until_stage_end(&env.stage_data) as usize;

            if until_end <= WIDTH {
                let vec = sample_envelope_vec_manual(env);
                return vec;
            } else {
                let val = sample_stage_vec(&env.stage_data);
                env.last_val = val[WIDTH - 1usize];
                return val;
            }
        }

        pub fn fill_envelope_vector(arr: *f32, len: usize) {
            let params = make_envelope_params();
            let env = make_envelope(params);

            let i = 0usize;

            while len - i >= WIDTH {
                let val = sample_envelope_vec(&env);
                store_vec::<<f32; WIDTH>>(&arr[i], val);
                i = i + WIDTH;
            }

            let last_val = sample_envelope_vec(&env);
            let j = 0usize;
            while i < len  {
                arr[i] = last_val[j];
                i = i + 1usize;
                j = j + 1usize;
            }
        }

        // struct Generator {
        //     envelope: Envelope,
        // }

        // fn generate_sample(*Generator) -> <f32; WIDTH> {
        //     let val = sample_envelope_vec(&env.envelope);
        //     val
        // }

        // struct Voice {
        //     generator: Generator,
        //     last_vec: <f32; WIDTH>,
        //     vec_pos: usize,
        // }

        // pub fn make_voice() -> *Voice {
        //     let params = make_envelope_params();
        //     let env = make_envelope(params);

        //     let generator = Generator {
        //         envelope: env,
        //     };

        //     let voice = Voice {
        //         generator: generator,
        //         last_vec: extend::<<f32; WIDTH>>(0.0f32),
        //         vec_pos: WIDTH,
        //     };

        //     return &voice;
        // }

        // pub fn gen_voice(voice: *Voice, arr: *f32, len: usize) {
        //     let params = make_envelope_params();
        //     let env = make_envelope(params);

        //     let i = 0usize;

        //     while len - i >= WIDTH {
        //         let val = sample_envelope_vec(&env);
        //         store_vec::<<f32; WIDTH>>(&arr[i], val);
        //         i = i + WIDTH;
        //     }

        //     let last_val = sample_envelope_vec(&env);
        //     let j = 0usize;
        //     while i < len  {
        //         arr[i] = last_val[j];
        //         i = i + 1usize;
        //         j = j + 1usize;
        //     }
        // }
    };

    println!("{}", &tokens);

    let tree = parse_tokens_to_tree(&tokens);

    println!("{:#?}", &tree);

    let module = mir_parse_module(&tree.unwrap()).unwrap();
    dbg!(&module);

    let mut ctx = LlvmCodegen::new();
    let module = ctx.insert_module(&module);

    dbg!("fns");
    module.print_functions();
    dbg!("optimizing");
    module.optimize();
    dbg!("optimized");
    module.print_functions();

    dbg!("writing to file");
    module.write_to_file();

    dbg!("running");

    let engine = module.execution_engine();

    let _arr = [1234u32, 2335u32];

    unsafe {
        let mut num_arr = vec![0f32; 100];
        let ptr = num_arr.as_mut_ptr();
        let compiled = engine
            .get_function::<unsafe extern "C" fn(*mut f32, usize)>("fill_envelope_vector")
            .unwrap();

        compiled.call(ptr, num_arr.len());
        println!("Result: {:?}", num_arr);
    };
}
