import { describe, it, expect } from 'bun:test'
import { TestUtilities } from '../../helpers/test-utilities'

describe('TestUtilities', () => {
	describe('assertGraphQLOutput', () => {
		it('should validate basic GraphQL output', () => {
			const sdl = 'type User { id: ID! name: String! } enum Status { ACTIVE INACTIVE }'

			expect(() => {
				TestUtilities.assertGraphQLOutput(sdl, {
					types: ['User'],
					enums: ['Status'],
				})
			}).not.toThrow()
		})

		it('should validate GraphQL types', () => {
			const sdl = 'type User { id: ID! } type Post { title: String! }'

			expect(() => {
				TestUtilities.assertGraphQLOutput(sdl, {
					types: ['User', 'Post'],
				})
			}).not.toThrow()
		})

		it('should validate GraphQL scalars', () => {
			const sdl = 'scalar DateTime scalar JSON type User { id: ID! }'

			expect(() => {
				TestUtilities.assertGraphQLOutput(sdl, {
					scalars: ['DateTime', 'JSON'],
					types: ['User'],
				})
			}).not.toThrow()
		})

		it('should validate GraphQL inputs', () => {
			const sdl = 'input UserInput { name: String } input PostInput { title: String }'

			expect(() => {
				TestUtilities.assertGraphQLOutput(sdl, {
					inputs: ['UserInput', 'PostInput'],
				})
			}).not.toThrow()
		})

		it('should validate type fields', () => {
			const sdl = 'type User { id: ID! name: String! email: String! }'

			expect(() => {
				TestUtilities.assertGraphQLOutput(sdl, {
					types: ['User'],
					fields: {
						User: ['id', 'name', 'email'],
					},
				})
			}).not.toThrow()
		})

		it('should validate content exclusions', () => {
			const sdl = 'type User { id: ID! name: String! }'

			expect(() => {
				TestUtilities.assertGraphQLOutput(sdl, {
					types: ['User'],
					notContains: ['Post', 'Comment'],
				})
			}).not.toThrow()
		})

		it('should handle undefined SDL', () => {
			expect(() => {
				TestUtilities.assertGraphQLOutput(undefined, {
					types: ['User'],
				})
			}).toThrow()
		})
	})

	describe('assertTypeScriptOutput', () => {
		it('should validate TypeScript classes', () => {
			const code = '@ObjectType() class User { @Field() name: string }'

			expect(() => {
				TestUtilities.assertTypeScriptOutput(code, {
					classes: ['User'],
					decorators: ['ObjectType', 'Field'],
				})
			}).not.toThrow()
		})

		it('should validate TypeScript imports', () => {
			const code = 'import { ObjectType, Field } from "type-graphql"'

			expect(() => {
				TestUtilities.assertTypeScriptOutput(code, {
					imports: ['import { ObjectType, Field } from "type-graphql"'],
				})
			}).not.toThrow()
		})

		it('should validate TypeScript interfaces', () => {
			const code = 'interface UserInput { name: string } interface PostInput { title: string }'

			expect(() => {
				TestUtilities.assertTypeScriptOutput(code, {
					interfaces: ['UserInput', 'PostInput'],
				})
			}).not.toThrow()
		})

		it('should handle undefined code', () => {
			expect(() => {
				TestUtilities.assertTypeScriptOutput(undefined, {
					classes: ['User'],
				})
			}).toThrow()
		})
	})

	describe('assertGenerationStats', () => {
		it('should validate exact statistics', () => {
			const stats = {
				objectTypes: 2,
				enumTypes: 1,
				scalarTypes: 3,
				inputTypes: 5,
				connectionTypes: 2,
				totalTypes: 13,
				generationTimeMs: 150,
			}

			expect(() => {
				TestUtilities.assertGenerationStats(stats, {
					objectTypes: 2,
					enumTypes: 1,
					scalarTypes: 3,
					inputTypes: 5,
					connectionTypes: 2,
					totalTypes: 13,
					generationTimeMs: 200,
				})
			}).not.toThrow()
		})

		it('should validate minimum values', () => {
			const stats = {
				objectTypes: 5,
				inputTypes: 10,
				generationTimeMs: 100,
			}

			expect(() => {
				TestUtilities.assertGenerationStats(stats, {
					minObjectTypes: 3,
					minInputTypes: 8,
				})
			}).not.toThrow()
		})

		it('should throw on incorrect exact values', () => {
			const stats = { objectTypes: 2 }

			expect(() => {
				TestUtilities.assertGenerationStats(stats, {
					objectTypes: 3,
				})
			}).toThrow()
		})
	})

	describe('assertArrayContains', () => {
		it('should validate array contains items', () => {
			const array = ['User', 'Post', 'Comment']

			expect(() => {
				TestUtilities.assertArrayContains(array, ['User', 'Post'])
			}).not.toThrow()
		})

		it('should throw when item not found', () => {
			const array = ['User', 'Post']

			expect(() => {
				TestUtilities.assertArrayContains(array, ['Comment'])
			}).toThrow()
		})
	})

	describe('assertArrayExactly', () => {
		it('should validate exact array match', () => {
			const array = ['User', 'Post']

			expect(() => {
				TestUtilities.assertArrayExactly(array, ['User', 'Post'])
			}).not.toThrow()
		})

		it('should validate exact array match regardless of order', () => {
			const array = ['User', 'Post']

			expect(() => {
				TestUtilities.assertArrayExactly(array, ['Post', 'User'])
			}).not.toThrow()
		})

		it('should throw on length mismatch', () => {
			const array = ['User', 'Post']

			expect(() => {
				TestUtilities.assertArrayExactly(array, ['User'])
			}).toThrow()
		})
	})

	describe('assertFieldMapping', () => {
		it('should validate field mappings', () => {
			const generatorResult = [
				{
					args: [
						'User',
						[
							{ name: 'id', type: 'String!', nullable: false, description: 'User ID' },
							{ name: 'name', type: 'String!', nullable: false, description: 'User name' },
						],
					],
				},
			]

			expect(() => {
				TestUtilities.assertFieldMapping(generatorResult, 'User', [
					{ name: 'id', type: 'String!', nullable: false, description: 'User ID' },
					{ name: 'name', type: 'String!', nullable: false, description: 'User name' },
				])
			}).not.toThrow()
		})

		it('should throw when model not found', () => {
			const generatorResult = []

			expect(() => {
				TestUtilities.assertFieldMapping(generatorResult, 'User', [])
			}).toThrow()
		})
	})

	describe('assertGeneratorResult', () => {
		it('should validate generator results', () => {
			const result = ['User', 'Post', 'UserInput', 'PostInput']

			expect(() => {
				TestUtilities.assertGeneratorResult(result, ['User', 'Post'])
			}).not.toThrow()
		})

		it('should validate empty results', () => {
			const result: string[] = []

			expect(() => {
				TestUtilities.assertGeneratorResult(result, [])
			}).not.toThrow()
		})

		it('should throw on invalid result types', () => {
			const result = [123, 'Post'] as any

			expect(() => {
				TestUtilities.assertGeneratorResult(result, [])
			}).toThrow()
		})
	})

	describe('createPerformanceThreshold', () => {
		it('should create performance threshold', () => {
			const threshold = TestUtilities.createPerformanceThreshold(1000)

			expect(threshold.maxTimeMs).toBe(1000)
			expect(typeof threshold.assertWithinThreshold).toBe('function')
		})

		it('should validate performance within threshold', () => {
			const threshold = TestUtilities.createPerformanceThreshold(1000)

			expect(() => {
				threshold.assertWithinThreshold(500)
			}).not.toThrow()
		})

		it('should throw when performance exceeds threshold', () => {
			const threshold = TestUtilities.createPerformanceThreshold(1000)

			expect(() => {
				threshold.assertWithinThreshold(1500)
			}).toThrow()
		})
	})
})
